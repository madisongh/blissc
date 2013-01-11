/*
 *++
 *	File:			gencode_llvm.c
 *
 *	Abstract:		Generates LLVM IR.
 *
 *  Module description:
 *
 *       This module generates LLVM from the expression tree.
 *
 *  All computations are performed on fullwords.  Registers are
 *  assumed to have fullword width.  All fetches return fullword
 *  values, so anything smaller needs to be 0- or sign-extended.
 *
 *  OPER_ASSIGN performs a store operation, which will need to
 *  truncate the value if the data segment has a smaller-than-fullword
 *  width.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		28-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */

#include <stdio.h>
#include "gencode.h"
#include "execfuncs.h"
#include "expression.h"
#include "symbols.h"
#include "machinedef.h"
#include "llvm-c/Core.h"
#include "llvm_helper.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#include <stdlib.h>
#include <assert.h>

struct gen_litsym_s {
    LLVMTypeRef     gentype;
    LLVMValueRef    genvalue;
};
typedef struct gen_litsym_s gen_litsym_t;

/*
 * A BLISS data segment can be one of:
 *  - register (i.e., not addressable memory)
 *  - a scalar stored in memory
 *  - just a block of memory (accessed via a structure)
 *  - a pointer (BIND or REF)
 *  - a pointer to a pointer (BIND to a REF)
 *
 * Registers cannot have their address taken; a fetch operation
 * simply returns the register contents (zero- or sign-extended
 * if needed).  Structures applied to registers are a bit more
 * complicated (and can only be used if the structure size fits
 * a fullword).
 *
 * Scalars can have a width that is less than a fullword,
 * and so need to be zero- or sign-extended when they are
 * fetched.
 */
struct gen_datasym_s {
    LLVMTypeRef         gentype;
    LLVMTypeRef         genptrtype;  // pointer to this, for non-REGs
    LLVMValueRef        value;
    LLVMValueRef        valueasint;
    LLVMValueRef        fn_for_intval; // track func valueasint was genned in
    LLVMTypeRef         reftype;
    LLVMTypeRef         refptrtype;
    int                 signext;
    enum { GDT_REG, GDT_MEM, GDT_PTR, GDT_PTR2PTR } type;
};
typedef struct gen_datasym_s gen_datasym_t;

struct gen_rtnsym_s {
    LLVMTypeRef     gentype;
    LLVMTypeRef     returntype;
    LLVMValueRef    genfn;
};
typedef struct gen_rtnsym_s gen_rtnsym_t;

struct gen_label_s {
    LLVMValueRef        phi;
    LLVMBasicBlockRef   exitpoint;
};
typedef struct gen_label_s gen_label_t;

struct gen_module_s {
    LLVMModuleRef       module;
};
typedef struct gen_module_s gen_module_t;


// Dispatch table for expression code generators

typedef int (*exprgen_fn)(gencodectx_t gctx, expr_node_t *);

#define DOEXPTYPE(typ_) \
    static int gencode_expr_##typ_(gencodectx_t gctx, expr_node_t *exp);
DOEXPTYPES
#undef DOEXPTYPE
#define DOEXPTYPE(typ_) gencode_expr_##typ_,
static exprgen_fn exprgen_dispatch[] = {
    DOEXPTYPES
};
#undef DOEXPTYPE

// Module context

#define BSTKSIZE 16
struct gencodectx_s {
    expr_ctx_t          ectx;
    symctx_t            symctx;
    machinedef_t        *mach;
    name_t              *modnp;
    unsigned int        tmpidx;
    unsigned int        lblidx;
    unsigned int        globidx;
    LLVMContextRef      llvmctx;
    LLVMModuleRef       module;
    LLVMTypeRef         novalue_type;
    LLVMTypeRef         fullword_type;
    LLVMTypeRef         fullword_pointer;
    LLVMTypeRef         onebit;
    LLVMTypeRef         unit_types[16];
    LLVMValueRef        curfn;
    LLVMValueRef        curfnphi;
    LLVMBasicBlockRef   curfnexit;
    LLVMValueRef        curblkphi;
    LLVMBasicBlockRef   curblkexit;
    LLVMValueRef        curloopphi;
    LLVMBasicBlockRef   curloopexit;
    LLVMBuilderRef      builder;
    LLVMPassManagerRef  passmgr;
    LLVMValueRef        memcpyfn;
    struct {
        LLVMBuilderRef  builder;
        unsigned int    tmpidx;
        unsigned int    lblidx;
        LLVMValueRef    curfn, curfnphi;
        LLVMBasicBlockRef curfnexit, curbldpos;
    }                   stack[BSTKSIZE];
    int                 bstkidx;
    char                tmpnambuf[NAME_SIZE];
};

static LLVMIntPredicate pred[] = {
    LLVMIntEQ, LLVMIntNE, LLVMIntSLT, LLVMIntSLE,
    LLVMIntSGT, LLVMIntSGE, LLVMIntEQ, LLVMIntNE,
    LLVMIntULT, LLVMIntULE, LLVMIntUGT, LLVMIntUGE
};

// Utility functions

static char *
gentempname (gencodectx_t gctx)
{
    snprintf(gctx->tmpnambuf, NAME_SIZE, "tmp.%u", gctx->tmpidx);
    gctx->tmpidx += 1;
    return gctx->tmpnambuf;
}
static char *
genglobname (gencodectx_t gctx)
{
    snprintf(gctx->tmpnambuf, NAME_SIZE, "$$$glob.%u", gctx->globidx);
    gctx->globidx += 1;
    return gctx->tmpnambuf;
}
static char *
genlabel (gencodectx_t gctx)
{
    snprintf(gctx->tmpnambuf, NAME_SIZE, "label.%u", gctx->lblidx);
    gctx->lblidx += 1;
    return gctx->tmpnambuf;
}

void
gendatatype (gencodectx_t gctx, data_attr_t *attr, gen_datasym_t *gd)
{
    // 1. Derive the base type
    gd->gentype = 0;
    if ((attr->flags & (SYM_M_REF|SYM_M_BIND)) != 0) {
        gd->reftype = gctx->fullword_type;
        if ((attr->flags & (SYM_M_REF|SYM_M_BIND)) == (SYM_M_REF|SYM_M_BIND)) {
            gd->type = GDT_PTR2PTR;
            gd->gentype = LLVMPointerType(LLVMPointerType(gctx->unit_types[0], 0), 0);
        } else {
            gd->gentype = LLVMPointerType(gctx->unit_types[0], 0);
            gd->type = GDT_PTR;
        }
    } else {
        if (attr->dclass == DCLASS_ARG || attr->dclass == DCLASS_REGISTER) {
            gd->type = GDT_REG;
        } else {
            gd->type = GDT_MEM;
        }
        // Structure: treat as array of units
        if (attr->struc != 0) {
            gd->gentype = LLVMArrayType(gctx->unit_types[0], attr->units);
            gd->reftype = gctx->fullword_type;
        } else {
            // Non-structure, must be a scalar or PLIT
            if (attr->units > machine_scalar_units(gctx->mach)) {
                gd->gentype = LLVMArrayType(gctx->unit_types[0], attr->units);
                gd->reftype = gctx->fullword_type;
            } else {
                assert(attr->units > 0);
                gd->gentype = gctx->unit_types[attr->units-1];
                gd->reftype = gd->gentype;
            }
        }
    }
    assert(gd->gentype != 0);
    gd->signext = (attr->flags & SYM_M_SIGNEXT) != 0;
    // 2. Derive the pointer-to-my-type, but not for registers
    gd->genptrtype = (gd->type == GDT_REG ? 0 : LLVMPointerType(gd->gentype, 0));
    gd->refptrtype = (gd->type == GDT_REG ? 0 : LLVMPointerType(gd->reftype, 0));
}

static LLVMTypeRef *
build_argtypes (gencodectx_t gctx, namereflist_t *argrefs)
{
    LLVMTypeRef *argtypes;
    nameref_t *arg;
    int i;
    
    if (namereflist_length(argrefs) == 0) {
        return 0;
    }
    argtypes = malloc(namereflist_length(argrefs)*sizeof(LLVMTypeRef));
    for (i = 0, arg = namereflist_head(argrefs);
         i < namereflist_length(argrefs); i++, arg = arg->tq_next) {
        data_attr_t *attr = datasym_attr(arg->np);
        gen_datasym_t *gd = sym_genspace(arg->np);
        gendatatype(gctx, attr, gd);
        argtypes[i] = gd->gentype;
    }
    
    return argtypes;
}

void
set_argnames (gencodectx_t gctx, LLVMValueRef thisfn, namereflist_t *argrefs)
{
    LLVMValueRef *args;
    nameref_t *arg;
    int i;
    
    if (namereflist_length(argrefs) == 0) {
        return;
    }
    args = malloc(namereflist_length(argrefs)*sizeof(LLVMValueRef));
    LLVMGetParams(thisfn, args);
    for (i = 0, arg = namereflist_head(argrefs);
         i < namereflist_length(argrefs); i++, arg = arg->tq_next) {
        gen_datasym_t *gd = sym_genspace(arg->np);
        LLVMSetValueName(args[i], name_azstring(arg->np));
        gd->value = args[i];
    }
    free(args);
}

static int
exprgen_gen (gencodectx_t gctx, expr_node_t *node) {

    return exprgen_dispatch[expr_type(node)](gctx, node);

}

static int
gencode_expr_NOOP (gencodectx_t gctx, expr_node_t *node)
{
    return 1;

} /* gencode_expr_NOOP */

static int
gencode_expr_PRIM_LIT (gencodectx_t gctx, expr_node_t *node)
{
    machinedef_t *mach = expr_machinedef(gctx->ectx);
    strdesc_t *str;
    LLVMValueRef val;

    str = expr_litstring(node);
    if (str == 0) {
        val = LLVMConstInt(gctx->fullword_type, expr_litval(node),
                           (machine_signext_supported(mach) ? 1 : 0));
    } else if (str->len <= machine_scalar_units(mach)) {
        unsigned long ival = 0;
        int i;
        for (i = (int)str->len; i >= 0; i--) {
            ival = (ival << 8) | str->ptr[i];
        }
        val = LLVMConstInt(gctx->fullword_type, ival, 0);
    } else {
        val = LLVMConstStringInContext(gctx->llvmctx,
                                       str->ptr, str->len, 1);
    }
    expr_genref_set(node, val);

    return 1;

} /* gencode_expr_PRIM_LIT */

static LLVMValueRef
gen_segment (gencodectx_t gctx, expr_node_t *node, int nogenasint)
{
    name_t *np = expr_seg_name(node);
    long offset = expr_seg_offset(node);
    LLVMValueRef val;

    switch (name_type(np)) {
        case LEXTYPE_NAME_DATA: {
            gen_datasym_t *gd = sym_genspace(np);
            val = gd->value;
            if (gd->type == GDT_REG) {
                expr_genref_set(node, val);
                return val;
            }
            if (nogenasint) {
                if (offset == 0) return val;
                if (gd->gentype == LLVMPointerType(gctx->unit_types[0], 0) ||
                    (LLVMGetTypeKind(gd->gentype) == LLVMArrayTypeKind &&
                     LLVMGetElementType(gd->gentype) == gctx->unit_types[0])) {
                    LLVMValueRef idx;
                    idx = LLVMConstInt(LLVMInt32TypeInContext(gctx->llvmctx), offset, 0);
                    val = LLVMBuildInBoundsGEP(gctx->builder, val, &idx, 1, gentempname(gctx));
                    return val;
                }
            }
            if (gd->valueasint == 0 || gctx->curfn != gd->fn_for_intval) {
                gd->valueasint = LLVMBuildPtrToInt(gctx->builder, gd->value,
                                                   gctx->fullword_type,
                                                   gentempname(gctx));
                gd->fn_for_intval = gctx->curfn;
            }
            val = gd->valueasint;
            break;
        }
        case LEXTYPE_NAME_ROUTINE: {
            gen_rtnsym_t *gr = sym_genspace(np);
            val = gr->genfn;
            break;
        }
        default:
            expr_signal(gctx->ectx, STC__INTCMPERR,
                        "gencode_expr_PRIM_SEG");
            return 0;
            break;
    }
    val = LLVMBuildPtrToInt(gctx->builder, val,
                            gctx->fullword_type, gentempname(gctx));
    if (offset != 0) {
        LLVMValueRef off = LLVMConstInt(gctx->fullword_type, offset, 1);
        val = LLVMBuildAdd(gctx->builder, val, off, gentempname(gctx));
    }
    if (nogenasint) {
        return LLVMBuildIntToPtr(gctx->builder, val, gctx->fullword_pointer,
                                 gentempname(gctx));
    }
    expr_genref_set(node, val);
    return val;
}

static int
gencode_expr_PRIM_SEG (gencodectx_t gctx, expr_node_t *node)
{
    return (gen_segment(gctx, node, 0) != 0);
}

static int
gencode_expr_PRIM_FLDREF (gencodectx_t gctx, expr_node_t *node)
{
    exprgen_gen(gctx, expr_fldref_pos(node));
    exprgen_gen(gctx, expr_fldref_size(node));
    exprgen_gen(gctx, expr_fldref_addr(node));
    expr_genref_set(node, expr_genref(expr_fldref_addr(node)));
    return 1;
}


static int
gencode_expr_PRIM_RTNCALL (gencodectx_t gctx, expr_node_t *node)
{
    expr_node_t *rtnexp = expr_rtnaddr(node);
    exprseq_t *args = expr_rtn_inargs(node);
    LLVMValueRef rtnadr, val, *argvals;
    LLVMTypeRef  type, rettype;

    if (expr_type(rtnexp) == EXPTYPE_PRIM_SEG &&
        name_type(expr_seg_name(rtnexp)) == LEXTYPE_NAME_ROUTINE) {
        name_t *np = expr_seg_name(rtnexp);
        gen_rtnsym_t *gr = sym_genspace(np);
        rtnadr = gr->genfn;
        type = gr->gentype;
        rettype = gr->returntype;
    } else {
        exprgen_gen(gctx, rtnexp);
        rettype = gctx->fullword_type;
        type = LLVMFunctionType(rettype, 0, 0, 1);
        rtnadr = expr_genref(rtnexp);
        if (LLVMGetTypeKind(LLVMTypeOf(rtnadr)) == LLVMPointerTypeKind) {
            rtnadr = LLVMBuildPointerCast(gctx->builder, rtnadr, type, gentempname(gctx));
        } else {
            rtnadr = LLVMBuildIntToPtr(gctx->builder, rtnadr, type, gentempname(gctx));
        }
    }
    if (exprseq_length(args) == 0) {
        argvals = 0;
    } else {
        int i;
        unsigned int numformals = LLVMCountParamTypes(type);
        expr_node_t *arg;
        LLVMTypeRef *ftypes;
        argvals = malloc(exprseq_length(args)*sizeof(LLVMValueRef));
        ftypes = (numformals == 0 ? 0 : malloc(numformals*sizeof(LLVMTypeRef)));
        if (numformals != 0) LLVMGetParamTypes(type, ftypes);
        for (i = 0, arg = exprseq_head(args); arg != 0; i++, arg = arg->tq_next) {
            exprgen_gen(gctx, arg);
            argvals[i] = expr_genref(arg);
            if (i < numformals && LLVMTypeOf(argvals[i]) != ftypes[i]) {
                if (LLVMGetTypeKind(LLVMTypeOf(argvals[i])) == LLVMPointerTypeKind) {
                    argvals[i] = LLVMBuildPointerCast(gctx->builder, argvals[i],
                                                      ftypes[i], gentempname(gctx));
                } else if (LLVMGetTypeKind(ftypes[i]) == LLVMPointerTypeKind) {
                    argvals[i] = LLVMBuildIntToPtr(gctx->builder, argvals[i],
                                                   ftypes[i], gentempname(gctx));
                } else {
                    argvals[i] = LLVMBuildIntCast(gctx->builder, argvals[i],
                                                  ftypes[i], gentempname(gctx));
                }
            }
        }
        if (ftypes != 0) free(ftypes);
    }
    val = LLVMBuildCall(gctx->builder, rtnadr, argvals, exprseq_length(args),
                        (LLVMGetTypeKind(rettype) == LLVMVoidTypeKind
                         ? "" : gentempname(gctx)));
    expr_genref_set(node, val);
    return 1;
}
static int
gencode_expr_PRIM_STRUREF (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef val;
    
    exprgen_gen(gctx, expr_struref_accexpr(node));
    val = expr_genref(expr_struref_accexpr(node));
    if (LLVMGetTypeKind(LLVMTypeOf(val)) != LLVMPointerTypeKind) {
        val = LLVMBuildIntToPtr(gctx->builder, val,
                                LLVMPointerType(gctx->unit_types[0], 0),
                                gentempname(gctx));
    }
    expr_genref_set(node, val);
    
    return 1;
}
static int
gencode_expr_PRIM_BLK (gencodectx_t gctx, expr_node_t *node)
{
    exprseq_t *seq = expr_blk_seq(node);
    namereflist_t *labels = expr_blk_labels(node);
    nameref_t *lbl;
    expr_node_t *exp;
    LLVMBasicBlockRef cbesave = gctx->curblkexit;
    LLVMValueRef cbpsave = gctx->curblkphi, normalval;
    
    if (namereflist_length(labels) > 0) {
        LLVMBasicBlockRef curblk, afterblk;
        curblk = LLVMGetInsertBlock(gctx->builder);
        afterblk = LLVMGetNextBasicBlock(curblk);
        lbl = namereflist_head(labels);
        if (afterblk == 0) {
            gctx->curblkexit = LLVMAppendBasicBlockInContext(gctx->llvmctx,
                                                             gctx->curfn,
                                                             name_azstring(lbl->np));
        } else {
            gctx->curblkexit = LLVMInsertBasicBlockInContext(gctx->llvmctx,
                                                             afterblk,
                                                             name_azstring(lbl->np));
        }
        LLVMPositionBuilderAtEnd(gctx->builder, gctx->curblkexit);
        if (expr_has_value(node)) {
            gctx->curblkphi = LLVMBuildPhi(gctx->builder,
                                           gctx->fullword_type,
                                           genlabel(gctx));
        }
        LLVMPositionBuilderAtEnd(gctx->builder, curblk);        
        for (lbl = namereflist_head(labels); lbl != 0; lbl = lbl->tq_next) {
            gen_label_t *gl = sym_genspace(lbl->np);
            gl->phi = gctx->curblkphi;
            gl->exitpoint = gctx->curblkexit;
        }
    }

    for (exp = exprseq_head(seq); exp != 0; exp = exp->tq_next) {
        exprgen_gen(gctx, exp);
    }
    
    if (expr_has_value(node)) {
        normalval = expr_genref(expr_blk_valexp(node));
    }
    if (namereflist_length(labels) > 0) {
        LLVMBasicBlockRef normalbb = LLVMGetPreviousBasicBlock(gctx->curblkexit);
        if (expr_has_value(node)) {
            LLVMAddIncoming(gctx->curblkphi, &normalval, &normalbb, 1);
            expr_genref_set(node, gctx->curblkphi);
            gctx->curblkphi = cbpsave;
        }
        if (LLVMGetBasicBlockTerminator(normalbb) == 0) {
            LLVMPositionBuilderAtEnd(gctx->builder, normalbb);
            LLVMBuildBr(gctx->builder, gctx->curblkexit);
        }
        LLVMPositionBuilderAtEnd(gctx->builder, gctx->curblkexit);
        gctx->curblkexit = cbesave;
    } else if (expr_has_value(node)) {
        expr_genref_set(node, normalval);
    }
    return 1;
}

static int
gencode_expr_OPERATOR (gencodectx_t gctx, expr_node_t *node)
{
    optype_t op = expr_op_type(node);
    expr_node_t *elhs, *erhs;
    char valname[NAME_SIZE];
    LLVMValueRef v, rhs, lhs;
    
    elhs = expr_op_lhs(node);
    erhs = expr_op_rhs(node);
    if (oper_is_unary(op) && op != OPER_FETCH) {
        exprgen_gen(gctx, erhs);
    } else if (op != OPER_FETCH) {
        if (op != OPER_ASSIGN) exprgen_gen(gctx, elhs);
        exprgen_gen(gctx, erhs);
    }
    v = 0;
    rhs = expr_genref(erhs);
    lhs = (elhs == 0 ? 0 : expr_genref(elhs));
    strcpy(valname, gentempname(gctx));
    switch (op) {
        case OPER_FETCH: {
            expr_node_t *base = erhs;
            if (expr_type(base) == EXPTYPE_PRIM_FLDREF) {
                base = expr_fldref_addr(base);
            }
            if (expr_type(base) == EXPTYPE_PRIM_SEG &&
                name_type(expr_seg_name(base)) == LEXTYPE_NAME_DATA) {
                gen_datasym_t *gd = sym_genspace(expr_seg_name(base));
                if (gd->type == GDT_REG) {
                    v = gd->value;
                } else {
                    if (gd->reftype != gd->gentype) {
                        v = LLVMBuildPointerCast(gctx->builder, gd->value,
                                                 gd->refptrtype, gentempname(gctx));
                        v = LLVMBuildLoad(gctx->builder, v, gentempname(gctx));
                    } else {
                        v = LLVMBuildLoad(gctx->builder, gd->value, gentempname(gctx));
                    }
                }
                if (gd->reftype != gctx->fullword_type) {
                    v = (gd->signext
                         ? LLVMBuildSExt(gctx->builder, v,
                                      gctx->fullword_type,
                                      valname)
                         : LLVMBuildZExt(gctx->builder, v,
                                      gctx->fullword_type, valname));
                }
                if (expr_type(erhs) == EXPTYPE_PRIM_FLDREF) {
                    exprgen_gen(gctx, expr_fldref_pos(erhs));
                    exprgen_gen(gctx, expr_fldref_size(erhs));
                }
            } else {
                exprgen_gen(gctx, erhs);
                rhs = expr_genref(erhs);
                if (LLVMGetTypeKind(LLVMTypeOf(rhs)) != LLVMPointerTypeKind) {
                    rhs = LLVMBuildIntToPtr(gctx->builder, rhs,
                                            gctx->fullword_pointer, gentempname(gctx));
                }
                v = LLVMBuildLoad(gctx->builder, rhs, valname);
            }
            if (expr_type(erhs) == EXPTYPE_PRIM_FLDREF) {
                LLVMValueRef mask, neg1;
                v = LLVMBuildLShr(gctx->builder, v,
                                  expr_genref(expr_fldref_pos(erhs)),
                                  gentempname(gctx));
                neg1 = LLVMConstAllOnes(gctx->fullword_type);
                mask = LLVMBuildShl(gctx->builder, neg1,
                                    expr_genref(expr_fldref_size(erhs)),
                                    gentempname(gctx));
                v = LLVMBuildAnd(gctx->builder, v, mask, gentempname(gctx));
                if (expr_fldref_signext(erhs)) {
                    v = LLVMBuildSExt(gctx->builder, v,
                                      gctx->fullword_type, gentempname(gctx));
                }
            }
            if (LLVMGetIntTypeWidth(LLVMTypeOf(v)) <
                machine_scalar_bits(gctx->mach)) {
                v = LLVMBuildZExt(gctx->builder, v, gctx->fullword_type,
                                  gentempname(gctx));
            }
            break;
        }
        case OPER_UNARY_PLUS:
            v = rhs;
            break;
        case OPER_UNARY_MINUS:
            v = LLVMBuildNeg(gctx->builder, rhs, valname);
            break;
        case OPER_ADD:
            v = LLVMBuildAdd(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_AND:
            v = LLVMBuildAnd(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_ASSIGN: {
            expr_node_t *base = elhs;
            const char *assignee;
            LLVMTypeRef  desttype;
            int dest_is_reg = 0;
            int dest_is_stru = 0;
            assert(base != 0);
            if (expr_type(base) == EXPTYPE_PRIM_FLDREF) {
                base = expr_fldref_addr(elhs);
            }
            if (expr_type(base) == EXPTYPE_PRIM_SEG &&
                expr_seg_offset(base) == 0 &&
                name_type(expr_seg_name(base)) == LEXTYPE_NAME_DATA) {
                gen_datasym_t *gd = sym_genspace(expr_seg_name(base));
                data_attr_t *attr = datasym_attr(expr_seg_name(base));
                LLVMTypeRef destptrtype;
                desttype = gd->reftype;
                destptrtype = gd->refptrtype;
                dest_is_reg = (gd->type == GDT_REG);
                dest_is_stru = (attr->struc != 0);
                lhs = gd->value;
                if (!dest_is_reg && destptrtype != gd->genptrtype) {
                    lhs = LLVMBuildPointerCast(gctx->builder, lhs, destptrtype,
                                               gentempname(gctx));
                }
                assignee = name_azstring(expr_seg_name(base));
                if (expr_type(elhs) == EXPTYPE_PRIM_FLDREF) {
                    exprgen_gen(gctx, expr_fldref_pos(elhs));
                    exprgen_gen(gctx, expr_fldref_size(elhs));
                }
            } else {
                exprgen_gen(gctx, elhs);
                lhs = expr_genref(elhs);
                assignee = LLVMGetValueName(expr_genref(base));
                desttype = gctx->fullword_type;
                lhs = LLVMBuildIntToPtr(gctx->builder, lhs,
                                        gctx->fullword_pointer, gentempname(gctx));
            }
            if (expr_type(elhs) == EXPTYPE_PRIM_FLDREF) {
                LLVMValueRef neg1, srcmask, dstmask, rs;
                LLVMValueRef size = expr_genref(expr_fldref_size(elhs));
                LLVMValueRef pos  = expr_genref(expr_fldref_pos(elhs));
                neg1 = LLVMConstAllOnes(gctx->fullword_type);
                v = LLVMBuildShl(gctx->builder, neg1, size, gentempname(gctx));
                srcmask = LLVMBuildNot(gctx->builder, v, gentempname(gctx));
                v = LLVMBuildShl(gctx->builder, srcmask, pos, gentempname(gctx));
                dstmask = LLVMBuildNot(gctx->builder, v, gentempname(gctx));
                if (dest_is_reg) {
                    if (desttype != gctx->fullword_type) {
                        v = LLVMBuildZExt(gctx->builder, lhs,
                                          gctx->fullword_type, gentempname(gctx));
                    } else {
                        v = lhs;
                    }
                } else {
                    if (dest_is_stru) {
                        lhs = LLVMBuildPointerCast(gctx->builder, lhs,
                                                 gctx->fullword_pointer, gentempname(gctx));
                        desttype = gctx->fullword_type;
                    }
                    v = LLVMBuildLoad(gctx->builder, lhs, gentempname(gctx));
                    if (desttype != gctx->fullword_type) {
                        v = LLVMBuildZExt(gctx->builder, v, gctx->fullword_type,
                                          gentempname(gctx));
                    }
                }
                v = LLVMBuildAnd(gctx->builder, v, dstmask, gentempname(gctx));
                if (desttype != gctx->fullword_type) {
                    v = LLVMBuildTrunc(gctx->builder, v, desttype, gentempname(gctx));
                }
                rs = LLVMBuildAnd(gctx->builder, rhs, srcmask, gentempname(gctx));
                rs = LLVMBuildShl(gctx->builder, rs, pos, gentempname(gctx));
                if (desttype != gctx->fullword_type) {
                    rs = LLVMBuildTrunc(gctx->builder, rs, desttype, gentempname(gctx));
                }
                if (dest_is_reg) {
                    LLVMBuildOr(gctx->builder, v, rs, assignee);
                } else {
                    v = LLVMBuildOr(gctx->builder, v, rs, gentempname(gctx));
                    LLVMBuildStore(gctx->builder, v, lhs);
                }
            } else {
                v = rhs;
                if (desttype != gctx->fullword_type) {
                    v = LLVMBuildTrunc(gctx->builder, rhs, desttype,
                                       (dest_is_reg ? assignee : gentempname(gctx)));
                    if (!dest_is_reg) {
                        LLVMBuildStore(gctx->builder, v, lhs);
                    }
                } else {
                    if (dest_is_reg) {
                        LLVMBuildOr(gctx->builder, v,
                                    LLVMConstInt(desttype, 0, 0), assignee);
                    } else {
                        LLVMBuildStore(gctx->builder, v, lhs);
                    }
                }
            }
            v = rhs;
            break;
        }
        case OPER_CMP_EQL:
        case OPER_CMP_GEQ:
        case OPER_CMP_GTR:
        case OPER_CMP_LEQ:
        case OPER_CMP_LSS:
        case OPER_CMP_NEQ:
        case OPER_CMP_EQLU:
        case OPER_CMP_GEQU:
        case OPER_CMP_GTRU:
        case OPER_CMP_LEQU:
        case OPER_CMP_LSSU:
        case OPER_CMP_NEQU:
        case OPER_CMP_EQLA:
        case OPER_CMP_GEQA:
        case OPER_CMP_GTRA:
        case OPER_CMP_LEQA:
        case OPER_CMP_LSSA:
        case OPER_CMP_NEQA:
            if (op >= OPER_CMP_EQLA && op <= OPER_CMP_GEQA) {
                op = op - (machine_addr_signed(gctx->mach) ? 12 : 6);
            }
            v = LLVMBuildICmp(gctx->builder, pred[op-OPER_CMP_EQL],
                              lhs, rhs, valname);
            v = LLVMBuildZExt(gctx->builder, v, gctx->fullword_type,
                              gentempname(gctx));
            break;
        case OPER_DIV:
            v = LLVMBuildUDiv(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_MODULO:
            v = LLVMBuildURem(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_MULT:
            v = LLVMBuildMul(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_NOT:
            v = LLVMBuildNot(gctx->builder, rhs, valname);
            break;
        case OPER_OR:
            v = LLVMBuildOr(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_SHIFT:
            // XXX
            v = LLVMBuildShl(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_SUBTRACT:
            v = LLVMBuildSub(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_XOR:
            v = LLVMBuildXor(gctx->builder, lhs, rhs, valname);
            break;
        case OPER_EQV:
            v = LLVMBuildXor(gctx->builder, lhs, rhs, gentempname(gctx));
            v = LLVMBuildNot(gctx->builder, v, valname);
            break;
    }
    
    expr_genref_set(node, v);
    
    return 1;
}
static int
gencode_expr_EXECFUN (gencodectx_t gctx, expr_node_t *node)
{
    strdesc_t *fname = name_string(expr_func_name(node));
    exprseq_t *args = expr_func_arglist(node);
    LLVMBasicBlockRef curblk, exitblk, afterblk;
    LLVMValueRef zero = LLVMConstNull(gctx->fullword_type);
    LLVMValueRef cmp, phi, v;

    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    if (afterblk == 0) {
        exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                genlabel(gctx));
    } else {
        exitblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);

    if (fname->len == 4 && memcmp(fname->ptr, "SIGN", 4) == 0) {
        LLVMBasicBlockRef posblk, negblk;
        LLVMValueRef pos1 = LLVMConstInt(gctx->fullword_type, 1, 1);
        LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
        posblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, genlabel(gctx));
        negblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, posblk, genlabel(gctx));
        exprgen_gen(gctx, exprseq_head(args));
        v = expr_genref(exprseq_head(args));
        cmp = LLVMBuildICmp(gctx->builder, LLVMIntEQ, v, zero, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, cmp, exitblk, negblk);
        LLVMAddIncoming(phi, &zero, &curblk, 1);
        LLVMPositionBuilderAtEnd(gctx->builder, negblk);
        cmp = LLVMBuildICmp(gctx->builder, LLVMIntSLT, v, zero, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, cmp, exitblk, posblk);
        LLVMAddIncoming(phi, &neg1, &negblk, 1);
        LLVMPositionBuilderAtEnd(gctx->builder, posblk);
        LLVMBuildBr(gctx->builder, exitblk);
        LLVMAddIncoming(phi, &pos1, &posblk, 1);
    } else if (fname->len == 3 && memcmp(fname->ptr, "ABS", 3) == 0) {
        LLVMBasicBlockRef negblk;
        negblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, genlabel(gctx));
        exprgen_gen(gctx, exprseq_head(args));
        v = expr_genref(exprseq_head(args));
        cmp = LLVMBuildICmp(gctx->builder, LLVMIntSLT, v, zero, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, cmp, negblk, exitblk);
        LLVMAddIncoming(phi, &v, &curblk, 1);
        LLVMPositionBuilderAtEnd(gctx->builder, negblk);
        v = LLVMBuildSub(gctx->builder, zero, v, gentempname(gctx));
        LLVMBuildBr(gctx->builder, exitblk);
        LLVMAddIncoming(phi, &v, &negblk, 1);
    } else {  // MAX or MIN
        expr_node_t *arg;
        LLVMIntPredicate p = (fname->ptr[1] == 'A' ? LLVMIntSGE : LLVMIntSLE);
        LLVMValueRef nextphi, v2;
        LLVMBasicBlockRef yesdest, nodest, here;

        if (fname->len == 4  && (fname->ptr[3] == 'U' ||
                                 !machine_addr_signed(gctx->mach))) {
            p = (fname->ptr[1] == 'A' ? LLVMIntUGE : LLVMIntULE);
        }
        arg = exprseq_head(args);
        exprgen_gen(gctx, arg);
        v = expr_genref(arg);

        here = LLVMGetInsertBlock(gctx->builder);
        for (arg = arg->tq_next; arg != 0; arg = arg->tq_next) {
            nodest = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                   genlabel(gctx));
            if (arg->tq_next == 0) {
                yesdest = exitblk;
                nextphi = phi;
            } else {
                yesdest = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                        genlabel(gctx));
                LLVMPositionBuilderAtEnd(gctx->builder, yesdest);
                nextphi = LLVMBuildPhi(gctx->builder, gctx->fullword_type,
                                       gentempname(gctx));
                LLVMPositionBuilderAtEnd(gctx->builder, here);
            }
            exprgen_gen(gctx, arg);
            v2 = expr_genref(arg);
            cmp = LLVMBuildICmp(gctx->builder, p, v, v2, gentempname(gctx));
            LLVMBuildCondBr(gctx->builder, cmp, yesdest, nodest);
            LLVMAddIncoming(nextphi, &v, &here, 1);
            LLVMPositionBuilderAtEnd(gctx->builder, nodest);
            LLVMBuildBr(gctx->builder, yesdest);
            LLVMAddIncoming(nextphi, &v2, &nodest, 1);
            LLVMPositionBuilderAtEnd(gctx->builder, yesdest);
            here = yesdest;
            v = nextphi;
        }
    }

    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    expr_genref_set(node, phi);

    return 1;
}

static int
gencode_expr_CTRL_COND (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, phi;
    LLVMBasicBlockRef curblk, consequent, alternative, afterblk, exitblk;
    int hasval = expr_has_value(node);

    alternative = 0;
    exprgen_gen(gctx, expr_cond_test(node));
    v = LLVMBuildTrunc(gctx->builder, expr_genref(expr_cond_test(node)), gctx->onebit, gentempname(gctx));
    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    if (afterblk == 0) {
        exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                genlabel(gctx));
    } else {
        exitblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    }
    consequent = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, genlabel(gctx));
    if (expr_cond_alternative(node)) {
        alternative = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                    genlabel(gctx));
    }
    if (hasval) {
        LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
        phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    LLVMBuildCondBr(gctx->builder, v, consequent,
                    (alternative == 0 ? exitblk : alternative));
    LLVMPositionBuilderAtEnd(gctx->builder, consequent);
    exprgen_gen(gctx, expr_cond_consequent(node));
    if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(gctx->builder)) == 0) {
        LLVMBuildBr(gctx->builder, exitblk);
        if (hasval) {
            v = expr_genref(expr_cond_consequent(node));
            LLVMAddIncoming(phi, &v, &consequent, 1);
        }
    }
    if (alternative != 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, alternative);
        exprgen_gen(gctx, expr_cond_alternative(node));
        if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(gctx->builder)) == 0) {
            LLVMBuildBr(gctx->builder, exitblk);
            if (hasval) {
                v = expr_genref(expr_cond_alternative(node));
                LLVMAddIncoming(phi, &v, &alternative, 1);
            }
        }
    }

    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);

    if (hasval) {
        expr_genref_set(node, phi);
    }
    
    return 1;
}

static int
gencode_expr_CTRL_CASE (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, phi, *xfervec, *destvec, xv, xvarray, loval, hival;
    LLVMValueRef caseindex, t1, t2, gepidx[2];
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
    LLVMBasicBlockRef curblk, afterblk, exitblk, last, *bbvec, testblk;
    expr_node_t **actions;
    long lo, hi, nactions, i, *cases;
    int hasval;
    unsigned int numcases;

    assert(node != 0);
    hasval = expr_has_value(node);
    lo = expr_case_lowbound(node);
    hi = expr_case_highbound(node);
    numcases  = (unsigned int) (hi-lo+1);
    nactions = expr_case_actioncount(node);
    if (nactions == 0) {
        return 1;
    }
    actions = expr_case_actions(node);
    cases = expr_case_cases(node);
    destvec = malloc(nactions*sizeof(LLVMValueRef));
    memset(destvec, 0, nactions*sizeof(LLVMValueRef));
    bbvec = malloc(nactions*sizeof(LLVMBasicBlockRef));
    memset(bbvec, 0, nactions*sizeof(LLVMBasicBlockRef));
    if (numcases != 0) {
        xfervec = malloc(numcases*sizeof(LLVMValueRef));
        memset(xfervec, 0, numcases*sizeof(LLVMValueRef));
    }
    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    if (afterblk == 0) {
        exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                genlabel(gctx));
    } else {
        exitblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    }
    if (hasval) {
        LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
        phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    }
    last = exitblk;

    for (i = nactions-1; i >= 0; i--) {
        LLVMBasicBlockRef here, here_end;
        here = LLVMInsertBasicBlockInContext(gctx->llvmctx, last, genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, here);
        exprgen_gen(gctx, actions[i]);
        here_end = LLVMGetInsertBlock(gctx->builder);
        if (LLVMGetBasicBlockTerminator(here_end) == 0) {
            if (hasval) {
                v = expr_genref(actions[i]);
                LLVMAddIncoming(phi, &v, &here_end, 1);
            }
            LLVMBuildBr(gctx->builder, exitblk);
        }
        destvec[i] = LLVMBlockAddress(gctx->curfn, here);
        bbvec[i] = here;
        last = here;
    }
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);

    if (numcases != 0) {
        for (i = lo; i <= hi; i++) {
            xfervec[i-lo] = destvec[cases[i-lo]];
        }
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, last, genlabel(gctx));
        xv = LLVMConstArray(LLVMTypeOf(destvec[0]), xfervec, numcases);
        xvarray = LLVMAddGlobal(gctx->module, LLVMTypeOf(xv), genglobname(gctx));
        LLVMSetLinkage(xvarray, LLVMPrivateLinkage);
        LLVMSetInitializer(xvarray, xv);
    }

    exprgen_gen(gctx, expr_case_index(node));
    caseindex = expr_genref(expr_case_index(node));
    i = expr_case_outrange(node);

    if (numcases == 0) {
        v = LLVMBuildBr(gctx->builder, (i >= 0 ? bbvec[i] : exitblk));
    } else {
        loval = LLVMConstInt(gctx->fullword_type, lo, 1);
        hival = LLVMConstInt(gctx->fullword_type, hi, 1);
        t1 = LLVMBuildICmp(gctx->builder, LLVMIntSLT, caseindex, loval,
                           gentempname(gctx));
        t2 = LLVMBuildICmp(gctx->builder, LLVMIntSGT, caseindex, hival,
                           gentempname(gctx));
        v = LLVMBuildOr(gctx->builder, t1, t2, gentempname(gctx));
        v = LLVMBuildCondBr(gctx->builder, v, (i >= 0 ? bbvec[i] : exitblk),
                            testblk);
    }
    if (hasval && i < 0) {
        LLVMAddIncoming(phi, &neg1, &curblk, 1);
    }

    if (numcases != 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, testblk);
        gepidx[0] = LLVMConstInt(gctx->fullword_type, 0, 0);
        gepidx[1] = LLVMBuildSub(gctx->builder, caseindex, loval, gentempname(gctx));
        v = LLVMBuildInBoundsGEP(gctx->builder, xvarray, gepidx, 2, gentempname(gctx));
        v = LLVMBuildLoad(gctx->builder, v, gentempname(gctx));
        v = LLVMBuildIndirectBr(gctx->builder, v, (unsigned int) nactions);
        for (i = 0; i < nactions; i++) LLVMAddDestination(v, bbvec[i]);
    }

    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    
    if (hasval) {
        expr_genref_set(node, phi);
    }

    free(bbvec);
    if (numcases != 0) free(xfervec);
    free(destvec);
    return 1;
}
static int
gencode_expr_CTRL_EXIT (gencodectx_t gctx, expr_node_t *node)
{
    name_t *np = expr_exit_label(node);
    expr_node_t *exitexp = expr_exit_value(node);
    LLVMValueRef exitval;
    
    if (exitexp != 0) {
        exprgen_gen(gctx, exitexp);
        exitval = expr_genref(exitexp);
    }
    
    if (np != 0) {
        gen_label_t *gl = sym_genspace(np);
        LLVMBasicBlockRef gotoblk = gl->exitpoint;
        if (exitexp != 0) {
            LLVMValueRef phi = gl->phi;
            LLVMBasicBlockRef here = LLVMGetInsertBlock(gctx->builder);
            LLVMAddIncoming(phi, &exitval, &here, 1);
        }
        LLVMBuildBr(gctx->builder, gotoblk);
    } else {
        LLVMBasicBlockRef here = LLVMGetInsertBlock(gctx->builder);
        if (exitexp == 0) {
            exitval = LLVMConstAllOnes(gctx->fullword_type);
        }
        LLVMAddIncoming(gctx->curloopphi, &exitval, &here, 1);
        LLVMBuildBr(gctx->builder, gctx->curloopexit);
    }

    return 1;
}
static int
gencode_expr_CTRL_RET (gencodectx_t gctx, expr_node_t *node)
{
    expr_node_t *exitval = expr_exit_value(node);
    LLVMValueRef val;
    LLVMBasicBlockRef thisblk;
    
    if (exitval == 0 || gctx->curfnphi == 0) {
        LLVMBuildBr(gctx->builder, gctx->curfnexit);
        return 1;
    }
    exprgen_gen(gctx, exitval);
    val = expr_genref(exitval);
    thisblk = LLVMGetInsertBlock(gctx->builder);
    LLVMAddIncoming(gctx->curfnphi, &val, &thisblk, 1);
    LLVMBuildBr(gctx->builder, gctx->curfnexit);
    return 1;
}
static int
gencode_expr_SELECTOR (gencodectx_t gctx, expr_node_t *node)
{
    // These should never be generated separately
    expr_signal(gctx->ectx, STC__INTCMPERR, "gencode_expr_SELECTOR");
    return 1;
}
static int
gencode_expr_CTRL_SELECT (gencodectx_t gctx, expr_node_t *node)
{
    int is_selectone = expr_sel_oneonly(node);
    optype_t op = expr_sel_cmptype(node);
    LLVMIntPredicate eqlpred = pred[op-OPER_CMP_EQL], geqpred, leqpred;
    exprseq_t *selseq = expr_sel_selectors(node);
    int hasval = expr_has_value(node);
    expr_node_t *sel, *always, *otherwise;
    LLVMBasicBlockRef curblk, afterblk, exitblk, lastblk, exitend;
    LLVMBasicBlockRef otherwiseblk, *testblks, *testends, *matchdests, *matchdestends;
    LLVMValueRef phi, v, indexval, *testvals;
    unsigned int numtests, i;

    // XXX Assumption about ordering in the operator enum
    geqpred = pred[(op+5)-OPER_CMP_EQL];
    leqpred = pred[(op+3)-OPER_CMP_EQL];

    always = expr_sel_alwaysaction(node);
    otherwise = expr_sel_otherwiseaction(node);

    exprgen_gen(gctx, expr_sel_index(node));
    indexval = expr_genref(expr_sel_index(node));
    curblk = LLVMGetInsertBlock(gctx->builder);

    afterblk = LLVMGetNextBasicBlock(curblk);
    if (afterblk == 0) {
        exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                genlabel(gctx));
    } else {
        exitblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    if (always != 0) {
        exprgen_gen(gctx, always);
        if (hasval) {
            phi = expr_genref(always);
        }
    } else if (hasval) {
        phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    }
    exitend = LLVMGetInsertBlock(gctx->builder);
    lastblk = exitblk;

    if (otherwise != 0) {
        LLVMBasicBlockRef o_end;
        otherwiseblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                     genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, otherwiseblk);
        exprgen_gen(gctx, otherwise);
        o_end = LLVMGetInsertBlock(gctx->builder);
        if (LLVMGetBasicBlockTerminator(o_end) == 0) {
            LLVMBuildBr(gctx->builder, exitblk);
            if (always == 0 && hasval) {
                v = expr_genref(otherwise);
                LLVMAddIncoming(phi, &v, &o_end, 1);
            }
        }
        lastblk = otherwiseblk;
    }

    numtests = exprseq_length(selseq);
    if (numtests == 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, curblk);
        if (otherwise == 0) {
            LLVMBuildBr(gctx->builder, exitblk);
            if (always == 0 && hasval) {
                v = LLVMConstAllOnes(gctx->fullword_type);
                LLVMAddIncoming(phi, &v, &curblk, 1);
            }
        } else {
            LLVMBuildBr(gctx->builder, otherwiseblk);
        }
        LLVMPositionBuilderAtEnd(gctx->builder, exitend);
        if (hasval) expr_genref_set(node, phi);
        return 1;
    }
    testblks = malloc(numtests*sizeof(LLVMBasicBlockRef));
    testends = malloc(numtests*sizeof(LLVMBasicBlockRef));
    matchdests = malloc(numtests*sizeof(LLVMBasicBlockRef));
    matchdestends = malloc(numtests*sizeof(LLVMBasicBlockRef));
    testvals = malloc(numtests*sizeof(LLVMValueRef));
    memset(testblks, 0, numtests*sizeof(LLVMBasicBlockRef));
    memset(testends, 0, numtests*sizeof(LLVMBasicBlockRef));
    memset(matchdests, 0, numtests*sizeof(LLVMBasicBlockRef));
    memset(matchdestends, 0, numtests*sizeof(LLVMBasicBlockRef));
    memset(testvals, 0, numtests*sizeof(LLVMValueRef));
    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        LLVMValueRef testval;
        expr_node_t *subsel;
        if (expr_selector_action(sel) == otherwise && otherwise != 0) {
            matchdests[i] = otherwiseblk;
        } else {
            matchdests[i] = LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                          genlabel(gctx));
            LLVMPositionBuilderAtEnd(gctx->builder, matchdests[i]);
            exprgen_gen(gctx, expr_selector_action(sel));
            matchdestends[i] = LLVMGetInsertBlock(gctx->builder);
            if (always == 0 && hasval && (is_selectone || sel->tq_next == 0) &&
                LLVMGetBasicBlockTerminator(matchdestends[i]) == 0) {
                v = expr_genref(expr_selector_action(sel));
                LLVMAddIncoming(phi, &v, &matchdestends[i], 1);
            }
            lastblk = matchdests[i];
        }
        testval = 0;
        testblks[i] = LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                    genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, testblks[i]);
        for (subsel = sel; subsel != 0; subsel = expr_selector_next(subsel)) {
            LLVMValueRef loval, hival;
            exprgen_gen(gctx, expr_selector_low(subsel));
            loval = expr_genref(expr_selector_low(subsel));
            if (expr_selector_high(subsel) == 0) {
                hival = 0;
            } else {
                exprgen_gen(gctx, expr_selector_high(subsel));
                hival = expr_genref(expr_selector_high(subsel));
            }
            v = LLVMBuildICmp(gctx->builder, (hival == 0 ? eqlpred : geqpred),
                              indexval, loval, gentempname(gctx));
            if (hival != 0) {
                LLVMValueRef hiv;
                hiv = LLVMBuildICmp(gctx->builder, leqpred,
                                    indexval, hival, gentempname(gctx));
                v = LLVMBuildAnd(gctx->builder, v, hiv, gentempname(gctx));
            }
            if (testval == 0) {
                testval = v;
            } else {
                testval = LLVMBuildOr(gctx->builder, testval, v,
                                      gentempname(gctx));
            }
        }
        testends[i] = LLVMGetInsertBlock(gctx->builder);
        testvals[i] = testval;
        lastblk = testblks[i];
    }

    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    LLVMBuildBr(gctx->builder, testblks[0]);

    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        LLVMBasicBlockRef nextifnomatch, nextaftermatchaction;
        if (sel->tq_next == 0) {
            if (otherwise == 0) {
                nextifnomatch = exitblk;
                if (hasval) {
                    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
                    LLVMAddIncoming(phi, &neg1, &testends[i], 1);
                }
            } else {
                nextifnomatch = otherwiseblk;
            }
            nextaftermatchaction = exitblk;
        } else {
            nextifnomatch = testblks[i+1];
            nextaftermatchaction = (is_selectone ? exitblk : testblks[i+1]);
        }
        LLVMPositionBuilderAtEnd(gctx->builder, testends[i]);
        LLVMBuildCondBr(gctx->builder, testvals[i], matchdests[i], nextifnomatch);
        if (otherwise == 0 || matchdests[i] != otherwiseblk) {
            if (LLVMGetBasicBlockTerminator(matchdestends[i]) == 0) {
                LLVMPositionBuilderAtEnd(gctx->builder, matchdestends[i]);
                LLVMBuildBr(gctx->builder, nextaftermatchaction);
            }
        }
    }
    LLVMPositionBuilderAtEnd(gctx->builder, exitend);
    if (hasval) expr_genref_set(node, phi);
    free(matchdests);
    free(matchdestends);
    free(testblks);
    free(testends);
    free(testvals);

    return 1;
}
static int
gencode_expr_CTRL_LOOPWU (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, saveclp;
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
    LLVMBasicBlockRef curblk, afterblk, savecle, loopblk, testblk;

    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    savecle = gctx->curloopexit;
    saveclp = gctx->curloopphi;
    if (afterblk == 0) {
        gctx->curloopexit = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                genlabel(gctx));
    } else {
        gctx->curloopexit = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk,
                                                          genlabel(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, gctx->curloopexit);
    gctx->curloopphi = LLVMBuildPhi(gctx->builder, gctx->fullword_type,
                                    gentempname(gctx));
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    if (expr_wuloop_type(node) == LOOP_PRETEST) {
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, gctx->curloopexit,
                                                genlabel(gctx));
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, genlabel(gctx));
        LLVMBuildBr(gctx->builder, testblk);
    } else {
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, gctx->curloopexit,
                                                genlabel(gctx));
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, testblk,
                                                genlabel(gctx));
        LLVMBuildBr(gctx->builder, loopblk);
    }
    LLVMPositionBuilderAtEnd(gctx->builder, testblk);
    exprgen_gen(gctx, expr_wuloop_test(node));
    v = LLVMBuildTrunc(gctx->builder, expr_genref(expr_wuloop_test(node)),
                       gctx->onebit, gentempname(gctx));
    LLVMBuildCondBr(gctx->builder, v, loopblk, gctx->curloopexit);
    LLVMPositionBuilderAtEnd(gctx->builder, loopblk);
    exprgen_gen(gctx, expr_wuloop_body(node));
    if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(gctx->builder)) == 0) {
        LLVMBuildBr(gctx->builder, testblk);
        LLVMAddIncoming(gctx->curloopphi, &neg1, &testblk, 1);
    }
    LLVMPositionBuilderAtEnd(gctx->builder, gctx->curloopexit);
    gctx->curloopexit = savecle;
    expr_genref_set(node, gctx->curloopphi);
    gctx->curloopphi = saveclp;
    return 1;
}
static int
gencode_expr_CTRL_LOOPID (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, saveclp, testphi, initval, endval, stepval;
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
    LLVMBasicBlockRef curblk, afterblk, savecle, loopblk, testblk;
    optype_t cmpop = expr_idloop_cmptype(node);
    gen_datasym_t *lindex = sym_genspace(expr_idloop_index(node));
    int is_decr;

    is_decr = (cmpop == OPER_CMP_GEQ || cmpop == OPER_CMP_GEQU ||
               cmpop == OPER_CMP_GEQA);
    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    savecle = gctx->curloopexit;
    saveclp = gctx->curloopphi;
    if (afterblk == 0) {
        gctx->curloopexit = LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn,
                                                          genlabel(gctx));
    } else {
        gctx->curloopexit = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk,
                                                          genlabel(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, gctx->curloopexit);
    gctx->curloopphi = LLVMBuildPhi(gctx->builder, gctx->fullword_type,
                                    gentempname(gctx));
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, gctx->curloopexit,
                                            genlabel(gctx));
    testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, genlabel(gctx));
    if (expr_idloop_init(node) != 0) {
        exprgen_gen(gctx, expr_idloop_init(node));
        initval = expr_genref(expr_idloop_init(node));
    } else {
        initval = (is_decr ? neg1 :  LLVMConstInt(gctx->fullword_type, 0, 0));
    }
    if (expr_idloop_term(node) != 0) {
        exprgen_gen(gctx, expr_idloop_term(node));
        endval = expr_genref(expr_idloop_term(node));
    } else {
        endval = (is_decr ? LLVMConstInt(gctx->fullword_type, 0, 0) : 0);
    }
    if (expr_idloop_step(node) != 0) {
        exprgen_gen(gctx, expr_idloop_step(node));
        stepval = expr_genref(expr_idloop_step(node));
    } else {
        stepval = LLVMConstInt(gctx->fullword_type, 1, 0);
    }
    LLVMBuildBr(gctx->builder, testblk);
    LLVMPositionBuilderAtEnd(gctx->builder, testblk);
    testphi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    LLVMBuildStore(gctx->builder, testphi, lindex->value);
    LLVMAddIncoming(testphi, &initval, &curblk, 1);
    if (endval == 0) {
        LLVMBuildBr(gctx->builder, loopblk);
    } else {
        v = LLVMBuildICmp(gctx->builder, pred[cmpop-OPER_CMP_EQL],
                          testphi, endval, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, v, loopblk, gctx->curloopexit);
        LLVMAddIncoming(gctx->curloopphi, &neg1, &testblk, 1);
    }
    LLVMPositionBuilderAtEnd(gctx->builder, loopblk);
    exprgen_gen(gctx, expr_idloop_body(node));
    if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(gctx->builder)) == 0) {
        v = LLVMBuildLoad(gctx->builder, lindex->value, gentempname(gctx));
        if (is_decr) {
            v = LLVMBuildSub(gctx->builder, v, stepval, gentempname(gctx));
        } else {
            v = LLVMBuildAdd(gctx->builder, v, stepval, gentempname(gctx));
        }
        LLVMBuildBr(gctx->builder, testblk);
        loopblk = LLVMGetInsertBlock(gctx->builder);
        LLVMAddIncoming(testphi, &v, &loopblk, 1);
    }

    expr_genref_set(node, gctx->curloopphi);
    gctx->curloopphi = saveclp;
    LLVMPositionBuilderAtEnd(gctx->builder, gctx->curloopexit);
    gctx->curloopexit = savecle;

    return 1;
}
// --- Begin API ---

/*
 * gencode_routine_begin
 */
int
gencode_routine_begin (gencodectx_t gctx, name_t *np)
{
    gen_rtnsym_t *gr = sym_genspace(np);
    LLVMValueRef thisfn = gr->genfn;
    routine_attr_t  *attr = rtnsym_attr(np);
    LLVMBasicBlockRef entryblk, exitblk;

    if (gctx->builder != 0) {
        int i = gctx->bstkidx;
        assert(gctx->bstkidx < BSTKSIZE);
        gctx->stack[i].curbldpos = LLVMGetInsertBlock(gctx->builder);
        gctx->stack[i].builder = gctx->builder;
        gctx->stack[i].tmpidx = gctx->tmpidx;
        gctx->stack[i].lblidx = gctx->lblidx;
        gctx->stack[i].curfn  = gctx->curfn;
        gctx->stack[i].curfnphi = gctx->curfnphi;
        gctx->stack[i].curfnexit = gctx->curfnexit;
        gctx->bstkidx += 1;
    }
    gctx->tmpidx = 0;
    gctx->lblidx = 0;
    entryblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                             genlabel(gctx));
    exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                            genlabel(gctx));
    gctx->builder = LLVMCreateBuilderInContext(gctx->llvmctx);
    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    if (attr->flags & SYM_M_NOVALUE) {
        LLVMBuildRetVoid(gctx->builder);
        gctx->curfnphi = 0;
    } else {
        gctx->curfnphi = LLVMBuildPhi(gctx->builder,
                                      gctx->fullword_type,
                                      gentempname(gctx));
        LLVMBuildRet(gctx->builder, gctx->curfnphi);
    }
    gctx->curfn = thisfn;
    gctx->curfnexit = exitblk;
    LLVMPositionBuilderAtEnd(gctx->builder, entryblk);

    return 1;
}

/*
 * gencode_routine_end
 */
int
gencode_routine_end (gencodectx_t gctx, name_t *np)
{
    gen_rtnsym_t *gr = sym_genspace(np);
    routine_attr_t *attr = rtnsym_attr(np);
    LLVMValueRef thisfn = gr->genfn;
    expr_node_t *rtnexpr = rtnsym_expr(np);
    LLVMBasicBlockRef lastinfn, prevbb;
    LLVMValueRef exprval;
    int do_branch = 1;

    gctx->curfn = thisfn;
    LLVMPositionBuilderAtEnd(gctx->builder, LLVMGetEntryBasicBlock(thisfn));
    exprgen_gen(gctx, rtnexpr);

    lastinfn = LLVMGetLastBasicBlock(thisfn);
    prevbb = LLVMGetPreviousBasicBlock(lastinfn);
    if ((attr->flags & SYM_M_NOVALUE) == 0) {
        exprval = expr_genref(rtnexpr);
        if (exprval != 0) {
            LLVMAddIncoming(gctx->curfnphi, &exprval, &prevbb, 1);
        } else if (LLVMGetBasicBlockTerminator(prevbb) == 0) {
            LLVMValueRef zero = LLVMConstInt(gctx->fullword_type, 0, 0);
            LLVMAddIncoming(gctx->curfnphi, &zero, &prevbb, 1);
        } else do_branch = 0;  // XXX assume it's already taken care of
    }
    if (do_branch) {
        LLVMPositionBuilderAtEnd(gctx->builder, prevbb);
        LLVMBuildBr(gctx->builder, gctx->curfnexit);
    }
    LLVMDisposeBuilder(gctx->builder);
    LLVMDumpValue(thisfn); fflush(stderr);
    LLVMVerifyFunction(thisfn, LLVMPrintMessageAction);
//    LLVMRunFunctionPassManager(gctx->passmgr, thisfn);
    if (gctx->bstkidx > 0) {
        int i = --gctx->bstkidx;
        gctx->builder = gctx->stack[i].builder;
        gctx->tmpidx = gctx->stack[i].tmpidx;
        gctx->lblidx = gctx->stack[i].lblidx;
        gctx->curfn = gctx->stack[i].curfn;
        gctx->curfnexit = gctx->stack[i].curfnexit;
        gctx->curfnphi = gctx->stack[i].curfnphi;
        LLVMPositionBuilderAtEnd(gctx->builder, gctx->stack[i].curbldpos);
    } else {
        gctx->builder = 0;
        gctx->curfn = 0;
        gctx->curfnphi = 0;
    }
    return 1;
}

/*
 * litsym_generator
 */
static int
litsym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    literal_attr_t *attr = litsym_attr(np);
    gen_litsym_t *gl = p, *ggl;
    namectx_t namectx = scope_namectx(name_scope(np));
    name_t *gnp;

    gnp = name_globalname(namectx, np);
    if (gnp != 0) {
        ggl = sym_genspace(gnp);
        if (ggl->gentype != 0) {
            gl->gentype = ggl->gentype;
            gl->genvalue = ggl->genvalue;
            return 1;
        }
    }

    gl->gentype = ((attr->width == machine_scalar_bits(gctx->mach))
                   ? gctx->fullword_type
                   : LLVMIntTypeInContext(gctx->llvmctx, attr->width));
    gl->genvalue = LLVMConstInt(gl->gentype, name_value_unsigned(np),
                              (attr->flags & SYM_M_SIGNEXT) != 0);
    if (gnp != 0) {
        LLVMValueRef gval;
        ggl->gentype = gl->gentype;
        ggl->genvalue = gl->genvalue;
        gval = LLVMAddGlobal(gctx->module, gl->gentype, name_azstring(np));
        LLVMSetInitializer(gval, gl->genvalue);
        LLVMSetGlobalConstant(gval, 1);
    }

    return 1;

} /* litsym_generator */

static char *
calculate_pointer (expr_node_t *pexp, char *base, unsigned int *lenp, int *signextp)
{
    char *ptr;

    if (expr_type(pexp) == EXPTYPE_PRIM_STRUREF) {
        pexp = expr_struref_accexpr(pexp);
    }
    if (expr_type(pexp) == EXPTYPE_PRIM_FLDREF) {
        expr_node_t *p = expr_fldref_pos(pexp), *s = expr_fldref_size(pexp);
        ptr = calculate_pointer(expr_fldref_addr(pexp), base, 0, 0);
        if (p != 0) {
            assert(expr_type(p) == EXPTYPE_PRIM_LIT);
            ptr += expr_litval(p);
        }
        assert(s != 0 && expr_type(s) == EXPTYPE_PRIM_LIT);
        if (lenp != 0) *lenp = (unsigned int)(expr_litval(s));
        if (signextp != 0) *signextp = expr_fldref_signext(pexp);
        return ptr;
    }
    assert(expr_type(pexp) == EXPTYPE_PRIM_SEG);
    if (lenp != 0) *lenp = expr_seg_units(pexp);
    if (signextp != 0) *signextp = expr_seg_signext(pexp);
    return base + expr_seg_offset(pexp);
}

static unsigned long
gen_initializer_const (gencodectx_t gctx, initval_t *iv, char **buf,
                       unsigned long segsize)
{
    char *cp, *itmbuf;
    int i, mustfree, is_preset;
    unsigned long len, allosize = 0;

    is_preset = iv->preset_expr != 0;

    if (*buf == 0) {
        allosize = (is_preset ? segsize : initval_size(gctx->symctx, iv));
        if (allosize < segsize) allosize = segsize;
        *buf = malloc(allosize);
        memset(*buf, 0, allosize);
    }

    cp = (is_preset ? 0 : *buf);

    while (iv != 0) {
        unsigned int targlen;
        mustfree = 0;
        if (is_preset) {
            assert(iv->repcount == 1);
            cp = calculate_pointer(iv->preset_expr, *buf, &targlen, 0);
        }
        itmbuf = 0;
        switch (iv->type) {
            case IVTYPE_LIST:
                assert(!is_preset);
                len = gen_initializer_const(gctx, iv->data.listptr, &itmbuf, 0);
                mustfree = 1;
                break;
            case IVTYPE_STRING:
                itmbuf = iv->data.string->ptr;
                len = iv->data.string->len;
                break;
            case IVTYPE_SCALAR: {
                int i;
                unsigned long val = iv->data.scalar.value;
                len = iv->data.scalar.width;
                itmbuf = malloc(len);
                memset(itmbuf, 0, len);
                for (i = 0; i < len && val != 0; i++) {
                    itmbuf[i] = val & 0xff;
                    val = val >> 8;
                }
                mustfree = 1;
                break;
            }
            case IVTYPE_EXPR_EXP:
                // not allowed
                len = 0;
                break;
        }
        if (is_preset && len > targlen) len = targlen;
        for (i = 0; i < iv->repcount; i++, cp += len) {
            memcpy(cp, itmbuf, len);
        }
        if (mustfree) free(itmbuf);
        iv = iv->next;
    }

    return (is_preset ? allosize : cp-*buf);

}

static LLVMValueRef
gen_initializer_llvmconst (gencodectx_t gctx, initval_t *iv,
                           unsigned int padcount,
                           LLVMTypeRef *ctype, int is_local)
{
    LLVMValueRef v, c, *varr;
    LLVMTypeRef  t = 0;
    initval_t *p;
    unsigned int count, i;

    for (p = iv, count = 0; p != 0; p = p->next, count++);
    if (padcount > 0) count += 1;
    varr = malloc(count * sizeof(LLVMValueRef));
    memset(varr, 0, count * sizeof(LLVMValueRef));

    for (i = 0; iv != 0; iv = iv->next, i++) {
        switch (iv->type) {
            case IVTYPE_LIST:
                varr[i] = gen_initializer_llvmconst(gctx, iv->data.listptr, 0, &t,
                                                    is_local);
                break;
            case IVTYPE_STRING:
                varr[i] = LLVMConstStringInContext(gctx->llvmctx, iv->data.string->ptr,
                                                   iv->data.string->len, 1);
                t = LLVMTypeOf(varr[i]);
                break;
            case IVTYPE_SCALAR:
                t = LLVMIntTypeInContext(gctx->llvmctx, iv->data.scalar.width * 8);
                varr[i] = LLVMConstInt(t, iv->data.scalar.value, iv->data.scalar.signext);
                break;
            case IVTYPE_EXPR_EXP: {
                expr_node_t *exp = iv->data.scalar.expr;
                if (expr_type(exp) == EXPTYPE_PRIM_SEG) {
                    varr[i] = gen_segment(gctx, exp, 1);
                    break;
                }
                exprgen_gen(gctx, exp);
                varr[i] = expr_genref(exp);
                break;
            }
        }

        if (iv->repcount > 1) {
            LLVMValueRef *subarray = malloc(iv->repcount*sizeof(LLVMValueRef));
            int j;
            memset(subarray, 0, iv->repcount*sizeof(LLVMValueRef));
            for (j = 0; j < iv->repcount; j++) subarray[j] = varr[i];
            varr[i] = LLVMConstArray(t, subarray, iv->repcount);
        }
    }

    if (padcount > 0) {
        if (padcount <= machine_scalar_units(gctx->mach)) {
            varr[count-1] = LLVMConstNull(gctx->unit_types[padcount-1]);
        } else {
            varr[count-1] = LLVMConstNull(LLVMArrayType(LLVMInt8TypeInContext(gctx->llvmctx), padcount));
        }
    }

    if (count == 1) {
//        if (is_local) {
            v = varr[0];
//        } else {
//            v = LLVMAddGlobal(gctx->module, LLVMTypeOf(varr[0]), genglobname(gctx));
//            LLVMSetLinkage(v, LLVMPrivateLinkage);
//            LLVMSetInitializer(v, varr[0]);
//        }
        *ctype = LLVMTypeOf(v);
        free(varr);
        return v;
    }

    c = LLVMConstStructInContext(gctx->llvmctx, varr, count, 1);
    if (is_local) {
        v = c;
    } else {
        v = LLVMAddGlobal(gctx->module, LLVMTypeOf(c), genglobname(gctx));
        LLVMSetLinkage(v, LLVMPrivateLinkage);
        LLVMSetInitializer(v, c);
    }
    *ctype = LLVMTypeOf(v);
    return v;

}


static LLVMValueRef
gen_initialized_datasym (gencodectx_t gctx, name_t *np, gen_datasym_t *gd,
                         data_attr_t *attr)
{
    LLVMValueRef dval, cval;
    LLVMTypeRef dtype;
    LLVMValueRef xyzval, zero = LLVMConstInt(gctx->fullword_type, 0, 0);
//    char *ivbuf = 0;
//    unsigned long ivlen;
//    int i;

//    if (gd->type == GDT_PTR || gd->type == GDT_PTR2PTR) {
        cval = gen_initializer_llvmconst(gctx, attr->ivlist, 0, &dtype, 0);
    xyzval = LLVMConstGEP(cval, &zero, 1);
//        if (gd->type == GDT_PTR || gd->type == GDT_PTR2PTR) {
 //           if (dtype != gd->gentype) {
 //               if (LLVMGetTypeKind(dtype) == LLVMPointerTypeKind) {
 //                   cval = LLVMConstPointerCast(cval, gd->gentype);
 //               } else {
 //                   cval = LLVMConstIntToPtr(cval, gd->gentype);
 //               }
 //           }
 //       }
//        dtype = gd->gentype;
//    } else {
//        ivlen = gen_initializer_const(gctx, attr->ivlist, &ivbuf, attr->units);
//        if (ivlen > machine_scalar_units(gctx->mach) || (attr->struc != 0)) {
//            cval = LLVMConstStringInContext(gctx->llvmctx, ivbuf, attr->units, 1);
//            dtype = LLVMArrayType(gctx->unit_types[0], attr->units);
//            assert(dtype == gd->gentype);
//        } else {
//            unsigned long val = 0;
//            for (i = (int)ivlen-1; i >= 0; i--) {
//                val = (val << 8) | ivbuf[i];
//            }
//            dtype = gd->gentype;
//            cval = LLVMConstInt(dtype, val, gd->signext);
//        }
//        free(ivbuf);
//    }
    dval = LLVMAddGlobal(gctx->module, dtype, name_azstring(np));
    gd->gentype = dtype;
    LLVMSetInitializer(dval, cval);
    return dval;
}

static void
gen_initial_assignments (gencodectx_t gctx, name_t *np, gen_datasym_t *gd,
                         data_attr_t *attr)
{
    LLVMValueRef cval, v, args[5];
    LLVMTypeRef ctype, byteptr;

    byteptr = LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0);
    if (attr->ivlist->preset_expr == 0) {
        unsigned int nunits, padcount;
        nunits = (unsigned int)initval_size(gctx->symctx, attr->ivlist);
        if (nunits > attr->units) {
            padcount = 0;
        } else {
            padcount = attr->units - nunits;
        }
        cval = gen_initializer_llvmconst(gctx, attr->ivlist, padcount, &ctype, 1);
        if (ctype != byteptr) {
            if (LLVMGetTypeKind(ctype) == LLVMPointerTypeKind) {
                cval = LLVMBuildPointerCast(gctx->builder, cval, byteptr,
                                            gentempname(gctx));
            } else {
                cval = LLVMBuildIntToPtr(gctx->builder, cval, byteptr,
                                         gentempname(gctx));
            }
        }
        if (gd->genptrtype != byteptr) {
            v = LLVMBuildPointerCast(gctx->builder, gd->value, byteptr,
                                     gentempname(gctx));
        } else {
            v = gd->value;
        }
        args[0] = v;
        args[1] = cval;
        args[2] = LLVMConstInt(LLVMInt32TypeInContext(gctx->llvmctx), attr->units, 0);
        args[3] = LLVMConstInt(LLVMInt32TypeInContext(gctx->llvmctx), 0, 0);
        args[4] = LLVMConstInt(LLVMInt1TypeInContext(gctx->llvmctx),
                               (attr->flags & SYM_M_VOLATILE) != 0, 0);
        LLVMBuildCall(gctx->builder, gctx->memcpyfn, args, 5, "");
    } else {
        expr_ctx_t ctx = gctx->ectx;
        textpos_t pos = parser_curpos(expr_parse_ctx(ctx));
        expr_node_t *exp;
        initval_t *iv;

        ctype = LLVMArrayType(LLVMInt8TypeInContext(gctx->llvmctx), attr->units);
        cval = LLVMAddGlobal(gctx->module, ctype, genglobname(gctx));
        LLVMSetLinkage(cval, LLVMPrivateLinkage);
        LLVMSetInitializer(cval, LLVMConstNull(ctype));
        cval = LLVMBuildPointerCast(gctx->builder, cval, byteptr, gentempname(gctx));
        if (gd->genptrtype != byteptr) {
            v = LLVMBuildPointerCast(gctx->builder, gd->value, byteptr, gentempname(gctx));
        } else {
            v = gd->value;
        }
        args[0] = v;
        args[1] = cval;
        args[2] = LLVMConstInt(LLVMInt32TypeInContext(gctx->llvmctx), attr->units, 0);
        args[3] = LLVMConstInt(LLVMInt32TypeInContext(gctx->llvmctx), 0, 0);
        args[4] = LLVMConstInt(LLVMInt1TypeInContext(gctx->llvmctx),
                               (attr->flags & SYM_M_VOLATILE) != 0, 0);
        LLVMBuildCall(gctx->builder, gctx->memcpyfn, args, 5, "");

        exp = expr_node_alloc(ctx, EXPTYPE_OPERATOR, pos);
        for (iv = attr->ivlist; iv != 0; iv = iv->next) {
            expr_op_lhs_set(exp, iv->preset_expr);
            expr_op_rhs_set(exp, iv->data.scalar.expr);
            expr_op_type_set(exp, OPER_ASSIGN);
            exprgen_gen(gctx, exp);
        }
        expr_op_lhs_set(exp, 0);
        expr_op_rhs_set(exp, 0);
        expr_node_free(ctx, exp);
    }
}
/*
 * datasym_generator
 */
static int
datasym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    namectx_t namectx = scope_namectx(name_scope(np));
    gen_datasym_t *gd = p;
    data_attr_t *attr = datasym_attr(np);
    char *namestr = name_azstring(np);

    if ((attr->flags & SYM_M_FORWARD) != 0 || attr->dclass == DCLASS_ARG) {
        return 1;
    }

    gendatatype(gctx, attr, gd);

    if (attr->dclass == DCLASS_STACKONLY || attr->dclass == DCLASS_STKORREG) {
        gd->value = LLVMBuildAlloca(gctx->builder, gd->gentype, namestr);
        if (attr->alignment != 0) {
            HelperSetAllocaAlignment(gd->value, 1<<attr->alignment);
        }
        if (attr->ivlist != 0) {
            gen_initial_assignments(gctx, np, gd, attr);
        }
    } else if (attr->dclass == DCLASS_STATIC) {
        if (attr->ivlist != 0) {
            gd->value = gen_initialized_datasym(gctx, np, gd, attr);
        } else {
            gd->value = LLVMAddGlobal(gctx->module, gd->gentype, namestr);
        }
        if (attr->owner != 0) {
            LLVMSetSection(gd->value, name_azstring(attr->owner));
        }
        if (name_globalname(namectx, np) == 0) {
            LLVMSetLinkage(gd->value, LLVMPrivateLinkage);
        }
        if (attr->alignment != 0) {
            LLVMSetAlignment(gd->value, 1<<attr->alignment);
        }
    }


    return 1;
    
}

/*
 * rtnsym_generator
 */
static int
rtnsym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    gen_rtnsym_t *gr = p;
    routine_attr_t *attr = rtnsym_attr(np);
    char *namestr = name_azstring(np);
    LLVMTypeRef *argtypes;
    LLVMValueRef thisfn, extantfn;
    int need_replace = 0;

    if (attr->flags & SYM_M_FORWARD) {
        // XXX
        gr->returntype = gctx->fullword_type;
        gr->gentype = LLVMFunctionType(gr->returntype, 0, 0, 1);
        gr->genfn = LLVMAddFunction(gctx->module, namestr, gr->gentype);
        return 1;
    }
    argtypes = build_argtypes(gctx, &attr->inargs);
    gr->returntype = ((attr->flags & SYM_M_NOVALUE) != 0
                      ? gctx->novalue_type
                      : gctx->fullword_type);
    gr->gentype = LLVMFunctionType(gr->returntype, argtypes,
                                   namereflist_length(&attr->inargs), 1);
    if (gr->gentype == 0) {
        return 0;
    }
    extantfn = LLVMGetNamedFunction(gctx->module, namestr);
    if (extantfn != 0 && LLVMTypeOf(extantfn) != gr->gentype) {
        LLVMSetValueName(extantfn, "");
        need_replace = 1;
    }
    thisfn = LLVMAddFunction(gctx->module, namestr, gr->gentype);
    if (need_replace) {
        LLVMValueRef castfn;
        castfn = LLVMBuildBitCast(gctx->builder, thisfn, LLVMTypeOf(extantfn), "");
        LLVMReplaceAllUsesWith(extantfn, castfn);
        LLVMDeleteFunction(extantfn);
    }
    if (name_globalname(expr_namectx(gctx->ectx), np) != 0) {
        LLVMSetLinkage(thisfn, LLVMExternalLinkage);
    } else {
        LLVMSetLinkage(thisfn, LLVMInternalLinkage);
    }
    if (attr->owner != 0) {
        LLVMSetSection(thisfn, name_azstring(attr->owner));
    }
    set_argnames(gctx, thisfn, &attr->inargs);
    gr->genfn = thisfn;

    return 1;

} /* gencode_rtnsym */

static int
symbol_generator (void *vctx, name_t *np, void *p)
{
    switch (name_type(np)) {
        case LEXTYPE_NAME_LITERAL:
            return litsym_generator(vctx, np, p);
        case LEXTYPE_NAME_DATA:
            return datasym_generator(vctx, np, p);
        case LEXTYPE_NAME_ROUTINE:
            return rtnsym_generator(vctx, np, p);
        default:
            return 1;
    }
}

static unsigned int
symbol_gensize (void *vctx, lextype_t lt) {
    switch (lt) {
        case LEXTYPE_NAME_DATA:
            return sizeof(gen_datasym_t);
        case LEXTYPE_NAME_ROUTINE:
            return sizeof(gen_rtnsym_t);
        case LEXTYPE_NAME_LABEL:
            return sizeof(gen_label_t);
        case LEXTYPE_NAME_MODULE:
            return sizeof(gen_module_t);
        case LEXTYPE_NAME_LITERAL:
            return sizeof(gen_litsym_t);
        default:
            return 0;
    }
}


/*
 * gencode_module_begin
 */
int
gencode_module_begin (gencodectx_t gctx, name_t *modnp)
{
    gen_module_t *gm = sym_genspace(modnp);
    LLVMTypeRef memcpytype;
    LLVMTypeRef memcpyargs[5];

    gctx->modnp = modnp;
    gctx->module = gm->module = LLVMModuleCreateWithNameInContext(name_azstring(modnp),
                                                                  gctx->llvmctx);
    gctx->passmgr = LLVMCreateFunctionPassManagerForModule(gctx->module);
    memcpyargs[0] = memcpyargs[1] = LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0);
    memcpyargs[2] = memcpyargs[3] = LLVMInt32TypeInContext(gctx->llvmctx);
    memcpyargs[4] = LLVMInt1TypeInContext(gctx->llvmctx);
    memcpytype = LLVMFunctionType(LLVMVoidTypeInContext(gctx->llvmctx), memcpyargs, 5, 0);
    gctx->memcpyfn = LLVMAddFunction(gctx->module, "llvm.memcpy.p0i8.p0i8.i32",
                                     memcpytype);

    LLVMAddBasicAliasAnalysisPass(gctx->passmgr);
    LLVMAddInstructionCombiningPass(gctx->passmgr);
    LLVMAddReassociatePass(gctx->passmgr);
    LLVMAddGVNPass(gctx->passmgr);
    LLVMAddCFGSimplificationPass(gctx->passmgr);

    return (gctx->module != 0);

} /* gencode_module_begin */

int
gencode_module_end (gencodectx_t gctx, name_t *np)
{
    gen_module_t *gm = sym_genspace(np);
    if (np != gctx->modnp || gm->module != gctx->module) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "gencode_module_end");
    }
    LLVMVerifyModule(gctx->module, LLVMPrintMessageAction, 0);
    LLVMDumpModule(gctx->module);
    LLVMDisposeModule(gctx->module);

    return 1;

} /* gencode_module_end */

/*
 * gencode_init
 *
 * Module initialization.
 */
gencodectx_t
gencode_init (void *ectx, logctx_t logctx, machinedef_t *mach, symctx_t symctx)
{
    gencodectx_t gctx = malloc(sizeof(struct gencodectx_s));

    static sym_genvec_t vec = {
        symbol_gensize, symbol_generator, 0, 0, 0
    };

    if (gctx == 0) return 0;
    memset(gctx, 0, sizeof(struct gencodectx_s));
    gctx->ectx = ectx;
    gctx->mach = mach;
    gctx->symctx = symctx;
    gctx->llvmctx = LLVMContextCreate();
    if (gctx->llvmctx == 0) {
        log_signal(logctx, 0, STC__INTCMPERR, "gencode_init");
        free(gctx);
        return 0;
    }
    symbols_gen_register(symctx, gctx, &vec);

    gctx->onebit = LLVMIntTypeInContext(gctx->llvmctx, 1);
    gctx->novalue_type = LLVMVoidTypeInContext(gctx->llvmctx);
    gctx->fullword_type = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(mach));
    gctx->fullword_pointer = LLVMPointerType(gctx->fullword_type, 0);
    if (machine_scalar_units(mach) == 1) {
        gctx->unit_types[0] = gctx->fullword_type;
    } else {
        unsigned int i;
        unsigned int bpunit = machine_unit_bits(mach);
        assert(machine_scalar_units(mach) <=
               sizeof(gctx->unit_types)/sizeof(gctx->unit_types[0]));
        for (i = 1; i < machine_scalar_units(mach); i++) {
            gctx->unit_types[i-1] = LLVMIntTypeInContext(gctx->llvmctx, bpunit*i);
        }
        gctx->unit_types[i-1] = gctx->fullword_type;
    }

    return gctx;

} /* gencode_init */

