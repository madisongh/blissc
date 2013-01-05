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
#include "expression.h"
#include "symbols.h"
#include "machinedef.h"
#include "llvm-c/Core.h"
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

struct gencodectx_s {
    expr_ctx_t          ectx;
    machinedef_t        *mach;
    name_t              *modnp;
    unsigned int        tmpidx;
    unsigned int        lblidx;
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
    char                tmpnambuf[NAME_SIZE];
    char                tmplblbuf[NAME_SIZE];
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
genlabel (gencodectx_t gctx)
{
    snprintf(gctx->tmplblbuf, NAME_SIZE, "label.%u", gctx->lblidx);
    gctx->lblidx += 1;
    return gctx->tmplblbuf;
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
            gd->gentype = LLVMPointerType(LLVMPointerType(gctx->fullword_type, 0), 0);
        } else {
            gd->gentype = LLVMPointerType(gctx->fullword_type, 0);
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
            // Non-structure, must be a scalar
            assert(attr->units > 0 && attr->units <= machine_scalar_units(gctx->mach));
            gd->gentype = gctx->unit_types[attr->units-1];
            gd->reftype = gd->gentype;
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
    } else {
        val = LLVMConstStringInContext(gctx->llvmctx,
                                       str->ptr, str->len, 1);
    }
    expr_genref_set(node, val);

    return 1;

} /* gencode_expr_PRIM_LIT */

static int
gencode_expr_PRIM_SEG (gencodectx_t gctx, expr_node_t *node)
{
    name_t *np = expr_seg_name(node);
    long offset = expr_seg_offset(node);
    LLVMValueRef val;
    LLVMTypeRef mytype;

    switch (name_type(np)) {
        case LEXTYPE_NAME_DATA: {
            gen_datasym_t *gd = sym_genspace(np);
            val = gd->value;
            if (gd->type == GDT_REG) {
                return 1;
            }
            mytype = gd->refptrtype;
            break;
        }
        case LEXTYPE_NAME_ROUTINE: {
            gen_rtnsym_t *gr = sym_genspace(np);
            val = gr->genfn;
            mytype = gctx->fullword_pointer;
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
    expr_genref_set(node, val);
    return 1;
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
        type = gr->gentype;
        rettype = gr->returntype;
        rtnadr = gr->genfn;
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
        expr_node_t *arg;
        argvals = malloc(exprseq_length(args)*sizeof(LLVMValueRef));
        for (i = 0, arg = exprseq_head(args); arg != 0; i++, arg = arg->tq_next) {
            exprgen_gen(gctx, arg);
            argvals[i] = expr_genref(arg);
        }
    }
    val = LLVMBuildCall(gctx->builder, rtnadr, argvals, exprseq_length(args),
                        gentempname(gctx));
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
                                gctx->fullword_pointer, gentempname(gctx));
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
    LLVMBasicBlockRef cbesave;
    LLVMValueRef cbpsave, normalval;
    
    if (namereflist_length(labels) > 0) {
        lbl = namereflist_head(labels);
        cbesave = gctx->curblkexit;
        gctx->curblkexit = LLVMAppendBasicBlockInContext(gctx->llvmctx,
                                                         gctx->curfn,
                                                         name_azstring(lbl->np));
        LLVMPositionBuilderAtEnd(gctx->builder, gctx->curblkexit);
        if (expr_has_value(node)) {
            cbpsave = gctx->curblkphi;
            gctx->curblkphi = LLVMBuildPhi(gctx->builder,
                                           gctx->fullword_type,
                                           genlabel(gctx));
        }
        LLVMPositionBuilderAtEnd(gctx->builder,
                                 LLVMGetPreviousBasicBlock(gctx->curblkexit));
        
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
        if (expr_has_value(node)) {
            LLVMBasicBlockRef normalbb = LLVMGetPreviousBasicBlock(gctx->curblkexit);
            LLVMAddIncoming(gctx->curblkphi, &normalval, &normalbb, 1);
            expr_genref_set(node, gctx->curblkphi);
            gctx->curblkphi = cbpsave;
        }
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
    if (oper_is_unary(op)) {
        exprgen_gen(gctx, erhs);
    } else {
        exprgen_gen(gctx, elhs);
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
            } else {
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
            LLVMTypeRef  desttype, destptrtype;
            int dest_is_reg = 0;
            int dest_is_stru = 0;
            if (expr_type(base) == EXPTYPE_PRIM_FLDREF) {
                base = expr_fldref_addr(elhs);
            }
            if (expr_type(base) == EXPTYPE_PRIM_SEG &&
                expr_seg_offset(base) == 0 &&
                name_type(expr_seg_name(base)) == LEXTYPE_NAME_DATA) {
                gen_datasym_t *gd = sym_genspace(expr_seg_name(base));
                data_attr_t *attr = datasym_attr(expr_seg_name(base));
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
            } else {
                assignee = LLVMGetValueName(expr_genref(base));
                desttype = gctx->fullword_type;
                destptrtype = gctx->fullword_pointer;
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
                    v = LLVMBuildOr(gctx->builder, v, rs, assignee);
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
                        v = LLVMBuildOr(gctx->builder, v,
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
    return 1;
}
static int
gencode_expr_CTRL_COND (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, phi;
    LLVMBasicBlockRef curblk, consequent, alternative, afterblk;
    int hasval = expr_has_value(node);

    alternative = 0;
    exprgen_gen(gctx, expr_cond_test(node));
    v = LLVMBuildTrunc(gctx->builder, expr_genref(expr_cond_test(node)), gctx->onebit, gentempname(gctx));
    curblk = LLVMGetInsertBlock(gctx->builder);
    afterblk = LLVMGetNextBasicBlock(curblk);
    afterblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    consequent = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    if (expr_cond_alternative(node)) {
        alternative = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk, genlabel(gctx));
    }
    if (hasval) {
        LLVMPositionBuilderAtEnd(gctx->builder, afterblk);
        phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    }
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    LLVMBuildCondBr(gctx->builder, v, consequent,
                    (alternative == 0 ? afterblk : alternative));
    LLVMPositionBuilderAtEnd(gctx->builder, consequent);
    exprgen_gen(gctx, expr_cond_consequent(node));
    LLVMBuildBr(gctx->builder, afterblk);
    if (hasval) {
        v = expr_genref(expr_cond_consequent(node));
        LLVMAddIncoming(phi, &v, &consequent, 1);
    }
    if (alternative != 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, alternative);
        exprgen_gen(gctx, expr_cond_alternative(node));
        LLVMBuildBr(gctx->builder, afterblk);
        if (hasval) {
            v = expr_genref(expr_cond_alternative(node));
            LLVMAddIncoming(phi, &v, &alternative, 1);
        }
    }
    LLVMPositionBuilderAtEnd(gctx->builder, afterblk);
    if (hasval) {
        expr_genref_set(node, phi);
    }
    
    return 1;
}
static int
gencode_expr_CTRL_CASE (gencodectx_t gctx, expr_node_t *node)
{
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
    
    if (exitval == 0) {
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
    return 1;
}
static int
gencode_expr_CTRL_SELECT (gencodectx_t gctx, expr_node_t *node)
{
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
    gctx->curloopexit = LLVMInsertBasicBlockInContext(gctx->llvmctx, afterblk,
                                                      genlabel(gctx));
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
    LLVMBuildBr(gctx->builder, testblk);
    LLVMAddIncoming(gctx->curloopphi, &neg1, &testblk, 1);
    gctx->curloopexit = savecle;
    expr_genref_set(node, gctx->curloopphi);
    gctx->curloopphi = saveclp;
    LLVMPositionBuilderAtEnd(gctx->builder, afterblk);
    return 1;
}
static int
gencode_expr_CTRL_LOOPID (gencodectx_t gctx, expr_node_t *node)
{
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
    
    entryblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                             genlabel(gctx));
    exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                            genlabel(gctx));
    gctx->builder = LLVMCreateBuilderInContext(gctx->llvmctx);
    LLVMPositionBuilderAtEnd(gctx->builder, exitblk);
    if (attr->flags & SYM_M_NOVALUE) {
        LLVMBuildRetVoid(gctx->builder);
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

    gctx->curfn = thisfn;
    LLVMPositionBuilderAtEnd(gctx->builder, LLVMGetEntryBasicBlock(thisfn));
    exprgen_gen(gctx, rtnexpr);

    lastinfn = LLVMGetLastBasicBlock(thisfn);
    prevbb = LLVMGetPreviousBasicBlock(lastinfn);
    if ((attr->flags & SYM_M_NOVALUE) == 0) {
        exprval = expr_genref(rtnexpr);
        LLVMAddIncoming(gctx->curfnphi, &exprval, &prevbb, 1);
    }
    LLVMPositionBuilderAtEnd(gctx->builder, prevbb);
    LLVMBuildBr(gctx->builder, gctx->curfnexit);
    LLVMDisposeBuilder(gctx->builder);
//    LLVMVerifyFunction(thisfn, LLVMPrintMessageAction);
//    LLVMRunFunctionPassManager(gctx->passmgr, thisfn);
    gctx->builder = 0;
    gctx->curfn = 0;
    gctx->curfnphi = 0;
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
    } else if (attr->dclass == DCLASS_STATIC) {
        gd->value = LLVMAddGlobal(gctx->module, gd->gentype, namestr);
        if (name_globalname(namectx, np) == 0) {
            LLVMSetVisibility(gd->value, LLVMHiddenVisibility);
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
    LLVMValueRef thisfn;

    if (attr->flags & SYM_M_FORWARD) {
        // XXX
        gr->returntype = gctx->fullword_type;
        gr->gentype = LLVMFunctionType(gr->returntype, 0, 0, 1);
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
    thisfn = LLVMGetNamedFunction(gctx->module, namestr);
    if (thisfn == 0) {
        thisfn = LLVMAddFunction(gctx->module, namestr, gr->gentype);
        if (name_globalname(expr_namectx(gctx->ectx), np) != 0) {
            LLVMSetLinkage(thisfn, LLVMExternalLinkage);
        } else {
            LLVMSetLinkage(thisfn, LLVMInternalLinkage);
        }
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

    gctx->modnp = modnp;
    gctx->module = gm->module = LLVMModuleCreateWithNameInContext(name_azstring(modnp),
                                                                  gctx->llvmctx);
    gctx->passmgr = LLVMCreateFunctionPassManagerForModule(gctx->module);

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
//    LLVMVerifyModule(gctx->module, LLVMPrintMessageAction, 0);
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

