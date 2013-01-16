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
#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm_helper.h"
#include <stdlib.h>
#include <assert.h>

struct branchtrack_s;
typedef struct branchtrack_s branchtrack_t;

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
    LLVMBasicBlockRef   block_for_intval;
    int                 signext, is_local;
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
    branchtrack_t       *btrack;
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
#define BLKPCMAX 256

struct branchtrack_s {
    struct branchtrack_s *next;
    LLVMBasicBlockRef exitblock;
    unsigned int branchcount;
    unsigned int phirefcount;
    LLVMBasicBlockRef phisource[BLKPCMAX];
    LLVMValueRef phival[BLKPCMAX];
};

struct gencodectx_s {
    expr_ctx_t          ectx;
    symctx_t            symctx;
    machinedef_t        *mach;
    name_t              *modnp;
    unsigned int        tmpidx;
    unsigned int        lblidx;
    unsigned int        globidx;
    LLVMContextRef      llvmctx;
    LLVMTargetMachineRef target_machine;
    LLVMModuleRef       module;
    LLVMTypeRef         novalue_type;
    LLVMTypeRef         fullword_type;
    LLVMTypeRef         fullword_pointer;
    LLVMTypeRef         onebit;
    LLVMTypeRef         unit_types[16];
    LLVMValueRef        curfn;
    branchtrack_t       *curfnbt;
    branchtrack_t       *curblkbt;
    branchtrack_t       *curloopbt;
    branchtrack_t       *freebts;
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
        branchtrack_t   *curfnbt, *curblkbt, *curloopbt;
        LLVMValueRef    curfn;
        LLVMBasicBlockRef curbldpos;
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

static const char *
llvm_section_for_psect (name_t *psname, int has_initializer, int is_extern)
{
    static const char *code_section = "__TEXT,__text";
    static const char *plit_section = "__TEXT,__const";
    static const char *own_section_init = "__DATA,__data";
    static const char *own_section_uninit = "__DATA,__bss";
    static const char *global_section_init = "__DATA,__data";
    static const char *global_section_uninit = "__DATA,__common";
    static const char *extern_section = "__DATA,__common";
    char *namestr = name_azstring(psname);

    if (strcmp(namestr, "$CODE$") == 0) {
        return code_section;
    } else if (strcmp(namestr, "$PLIT$") == 0) {
        return plit_section;
    } else if (strcmp(namestr, "$OWN$") == 0) {
        return (has_initializer ? own_section_init : own_section_uninit);
    } else if (is_extern) {
        return extern_section;
    } else return (has_initializer ? global_section_init : global_section_uninit);
}

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

static LLVMBasicBlockRef
create_exitblock (gencodectx_t gctx, char *label)
{
    LLVMBasicBlockRef insertpoint;
    char *l = (label == 0 ? genlabel(gctx) : label);
    insertpoint = LLVMGetNextBasicBlock(LLVMGetInsertBlock(gctx->builder));
    return (insertpoint == 0
            ? LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn, l)
            : LLVMInsertBasicBlockInContext(gctx->llvmctx, insertpoint, l));
}

static branchtrack_t *
new_btrack (gencodectx_t gctx, LLVMBasicBlockRef exitpoint) {
    branchtrack_t *bt;
    if (gctx->freebts == 0) {
        bt = malloc(sizeof(branchtrack_t));
    } else {
        bt = gctx->freebts;
        gctx->freebts = bt->next;
    }
    memset(bt, 0, sizeof(branchtrack_t));
    bt->exitblock = exitpoint;
    return bt;
}

static void
update_btrack_phi (gencodectx_t gctx, branchtrack_t *bt,
                   LLVMBasicBlockRef here, LLVMValueRef val)
{
    if (here == 0) here = LLVMGetInsertBlock(gctx->builder);
    bt->phisource[bt->phirefcount] = here;
    bt->phival[bt->phirefcount] = val;
    bt->phirefcount += 1;
}

static void
update_btrack (gencodectx_t gctx, branchtrack_t *bt, LLVMValueRef val)
{
    LLVMBasicBlockRef here = LLVMGetInsertBlock(gctx->builder);
    if (LLVMGetBasicBlockTerminator(here) == 0) {
        bt->branchcount += 1;
        LLVMBuildBr(gctx->builder, bt->exitblock);
        if (val != 0) update_btrack_phi(gctx, bt, here, val);
    }
}

static void
free_btrack (gencodectx_t gctx, branchtrack_t *bt)
{
    bt->next = gctx->freebts;
    gctx->freebts = bt;
}

static void
finalize_btrack (gencodectx_t gctx, branchtrack_t *bt, expr_node_t *node)
{
    if (bt == 0) return;
    if (bt->branchcount == 0) {
        LLVMDeleteBasicBlock(bt->exitblock);
    } else {
        LLVMPositionBuilderAtEnd(gctx->builder, bt->exitblock);
        if (bt->phirefcount > 0 && bt->phirefcount == bt->branchcount) {
            LLVMValueRef phi;
            phi = LLVMBuildPhi(gctx->builder, gctx->fullword_type,
                               gentempname(gctx));
            LLVMAddIncoming(phi, bt->phival, bt->phisource, bt->phirefcount);
            expr_genref_set(node, phi);
        }
    }
    free_btrack(gctx, bt);
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
            LLVMBasicBlockRef here;

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
            if (gd->is_local) {
                here = LLVMGetInsertBlock(gctx->builder);
            }
            if (gd->valueasint == 0 ||
                (!gd->is_local && gctx->curfn != gd->fn_for_intval) ||
                (gd->is_local && here != gd->block_for_intval)) {
                gd->valueasint = LLVMBuildPtrToInt(gctx->builder, gd->value,
                                                   gctx->fullword_type,
                                                   gentempname(gctx));
                if (gd->is_local) {
                    gd->block_for_intval = here;
                } else {
                    gd->fn_for_intval = gctx->curfn;
                }
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
    expr_genref_set(node, (LLVMGetTypeKind(rettype) == LLVMVoidTypeKind ? 0 : val));
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
    branchtrack_t *bt;
    LLVMValueRef normalval = 0;

    bt = 0;
    if (namereflist_length(labels) > 0) {
        LLVMBasicBlockRef exitblk, curblk;
        curblk = LLVMGetInsertBlock(gctx->builder);
        lbl = namereflist_head(labels);
        exitblk = create_exitblock(gctx, name_azstring(lbl->np));
        bt = new_btrack(gctx, exitblk);
        for (lbl = namereflist_head(labels); lbl != 0; lbl = lbl->tq_next) {
            gen_label_t *gl = sym_genspace(lbl->np);
            gl->btrack = bt;
        }
        bt->next = gctx->curblkbt;
        gctx->curblkbt = bt;
    }

    for (exp = exprseq_head(seq); exp != 0; exp = exp->tq_next) {
        exprgen_gen(gctx, exp);
    }

    if (expr_blk_valexp(node) != 0) {
        normalval = expr_genref(expr_blk_valexp(node));
        if (bt != 0) {
            update_btrack(gctx, bt, normalval);
        }
    }

    expr_genref_set(node, normalval);
    if (bt != 0) {
        gctx->curblkbt = bt->next;
        bt->next = 0;
        finalize_btrack(gctx, bt, node);
        for (lbl = namereflist_head(labels); lbl != 0; lbl = lbl->tq_next) {
            gen_label_t *gl = sym_genspace(lbl->np);
            gl->btrack = 0;
        }
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
                if (expr_type(expr_fldref_size(erhs)) == EXPTYPE_PRIM_LIT) {
                    unsigned int nbits = (unsigned int)expr_litval(expr_fldref_size(erhs));
                    LLVMTypeRef ftype = LLVMIntTypeInContext(gctx->llvmctx, nbits);
                    if (nbits < machine_scalar_units(gctx->mach)) {
                        v = LLVMBuildTrunc(gctx->builder, v, ftype, gentempname(gctx));
                        if (expr_fldref_signext(erhs)) {
                            v = LLVMBuildSExt(gctx->builder, v, gctx->fullword_type, gentempname(gctx));
                        } else {
                            v = LLVMBuildZExt(gctx->builder, v, gctx->fullword_type, gentempname(gctx));
                        }
                    }
                } else {
                    neg1 = LLVMConstAllOnes(gctx->fullword_type);
                    mask = LLVMBuildShl(gctx->builder, neg1,
                                        expr_genref(expr_fldref_size(erhs)),
                                        gentempname(gctx));
                    mask = LLVMBuildNeg(gctx->builder, mask, gentempname(gctx));
                    v = LLVMBuildAnd(gctx->builder, v, mask, gentempname(gctx));
                    if (expr_fldref_signext(erhs)) {
                        v = LLVMBuildSExt(gctx->builder, v,
                                          gctx->fullword_type, gentempname(gctx));
                    }
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
    LLVMBasicBlockRef exitblk;
    LLVMValueRef zero = LLVMConstNull(gctx->fullword_type);
    LLVMValueRef cmp, v;
    branchtrack_t *bt;

    exitblk = create_exitblock(gctx, 0);
    bt = new_btrack(gctx, exitblk);

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
        bt->branchcount += 1;
        update_btrack_phi(gctx, bt, 0, zero);
        LLVMPositionBuilderAtEnd(gctx->builder, negblk);
        cmp = LLVMBuildICmp(gctx->builder, LLVMIntSLT, v, zero, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, cmp, exitblk, posblk);
        bt->branchcount += 1;
        update_btrack_phi(gctx, bt, 0, neg1);
        LLVMPositionBuilderAtEnd(gctx->builder, posblk);
        update_btrack(gctx, bt, pos1);
    } else if (fname->len == 3 && memcmp(fname->ptr, "ABS", 3) == 0) {
        LLVMBasicBlockRef negblk;
        negblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, genlabel(gctx));
        exprgen_gen(gctx, exprseq_head(args));
        v = expr_genref(exprseq_head(args));
        cmp = LLVMBuildICmp(gctx->builder, LLVMIntSLT, v, zero, gentempname(gctx));
        LLVMBuildCondBr(gctx->builder, cmp, negblk, exitblk);
        bt->branchcount += 1;
        update_btrack_phi(gctx, bt, 0, v);
        LLVMPositionBuilderAtEnd(gctx->builder, negblk);
        v = LLVMBuildSub(gctx->builder, zero, v, gentempname(gctx));
        update_btrack(gctx, bt, v);
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
                nextphi = 0;
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
            here = LLVMGetInsertBlock(gctx->builder);
            if (yesdest == exitblk) {
                update_btrack_phi(gctx, bt, here, v);
                bt->branchcount += 1;
            } else {
                LLVMAddIncoming(nextphi, &v, &here, 1);
            }
            LLVMPositionBuilderAtEnd(gctx->builder, nodest);
            if (yesdest == exitblk) {
                update_btrack(gctx, bt, v2);
            } else {
                LLVMBuildBr(gctx->builder, yesdest);
                LLVMAddIncoming(nextphi, &v2, &nodest, 1);
            }
            LLVMPositionBuilderAtEnd(gctx->builder, yesdest);
            here = yesdest;
            v = nextphi;
        }
    }

    finalize_btrack(gctx, bt, node);

    return 1;
}

static int
gencode_expr_CTRL_COND (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v;
    LLVMBasicBlockRef consequent, alternative, exitblk;
    branchtrack_t *bt;
    int hasval = expr_has_value(node);

    alternative = 0;
    exprgen_gen(gctx, expr_cond_test(node));
    v = LLVMBuildTrunc(gctx->builder, expr_genref(expr_cond_test(node)), gctx->onebit, gentempname(gctx));
    exitblk = create_exitblock(gctx, 0);
    bt = new_btrack(gctx, exitblk);
    if (expr_cond_alternative(node)) {
        alternative = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                    genlabel(gctx));
    }
    consequent = LLVMInsertBasicBlockInContext(gctx->llvmctx,
                                               (alternative == 0
                                                ? exitblk : alternative),
                                               genlabel(gctx));
    LLVMBuildCondBr(gctx->builder, v, consequent,
                    (alternative == 0 ? exitblk : alternative));
    if (alternative == 0) bt->branchcount += 1;
    LLVMPositionBuilderAtEnd(gctx->builder, consequent);
    exprgen_gen(gctx, expr_cond_consequent(node));
    update_btrack(gctx, bt,
                  (hasval ? expr_genref(expr_cond_consequent(node)) : 0));
    if (alternative != 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, alternative);
        exprgen_gen(gctx, expr_cond_alternative(node));
        update_btrack(gctx, bt,
                      (hasval ? expr_genref(expr_cond_alternative(node)) : 0));
    }

    finalize_btrack(gctx, bt, node);

    return 1;
}

static int
gencode_expr_CTRL_CASE (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef sw, caseindex;
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
    LLVMBasicBlockRef curblk, exitblk, last, *bbvec;
    branchtrack_t *bt;
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
    bbvec = malloc(nactions*sizeof(LLVMBasicBlockRef));
    memset(bbvec, 0, nactions*sizeof(LLVMBasicBlockRef));
    exitblk = create_exitblock(gctx, 0);
    bt = new_btrack(gctx, exitblk);
    last = exitblk;
    exprgen_gen(gctx, expr_case_index(node));
    caseindex = LLVMBuildSub(gctx->builder, expr_genref(expr_case_index(node)),
                             LLVMConstInt(gctx->fullword_type, lo, 1),
                             gentempname(gctx));
    curblk = LLVMGetInsertBlock(gctx->builder);

    for (i = nactions-1; i >= 0; i--) {
        bbvec[i] = LLVMInsertBasicBlockInContext(gctx->llvmctx, last,
                                                 genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, bbvec[i]);
        exprgen_gen(gctx, actions[i]);
        update_btrack(gctx, bt, (hasval ? expr_genref(actions[i]) : 0));
        last = bbvec[i];
    }
    LLVMPositionBuilderAtEnd(gctx->builder, curblk);
    i = expr_case_outrange(node);
    sw = LLVMBuildSwitch(gctx->builder, caseindex,
                         (i < 0 ? exitblk : bbvec[i]), numcases);
    if (i < 0) {
        bt->branchcount += 1;
        if (hasval) update_btrack_phi(gctx, bt, curblk, neg1);
    }
    for (i = 0; i < numcases; i++) {
        LLVMAddCase(sw, LLVMConstInt(gctx->fullword_type, i, 0), bbvec[cases[i]]);
    }
    finalize_btrack(gctx, bt, node);
    free(bbvec);
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
        update_btrack(gctx, gl->btrack, (exitexp == 0 ? 0 : exitval));
    } else {
        update_btrack(gctx, gctx->curloopbt, (exitexp == 0 ? 0 : exitval));
    }

    return 1;
}
static int
gencode_expr_CTRL_RET (gencodectx_t gctx, expr_node_t *node)
{
    expr_node_t *exitexp = expr_exit_value(node);

    if (exitexp != 0) {
        exprgen_gen(gctx, exitexp);
    }
    update_btrack(gctx, gctx->curfnbt, (exitexp == 0 ? 0 : expr_genref(exitexp)));

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
    expr_node_t *sel, *always, *otherwise;
    LLVMBasicBlockRef curblk, exitblk, lastblk;
    LLVMBasicBlockRef otherwiseblk, alwaysblk;
    LLVMValueRef v, indexval;
    struct seltrack_s {
        LLVMValueRef testval, matchval;
        LLVMBasicBlockRef testblk, testend;
        LLVMBasicBlockRef matchdest, matchdestend;
    } *st;
    branchtrack_t *bt;
    unsigned int numtests, i;

    // XXX Assumption about ordering in the operator enum
    geqpred = pred[(op+5)-OPER_CMP_EQL];
    leqpred = pred[(op+3)-OPER_CMP_EQL];

    always = expr_sel_alwaysaction(node);
    otherwise = expr_sel_otherwiseaction(node);

    exprgen_gen(gctx, expr_sel_index(node));
    indexval = expr_genref(expr_sel_index(node));
    curblk = LLVMGetInsertBlock(gctx->builder);
    exitblk = create_exitblock(gctx, 0);
    bt = new_btrack(gctx, exitblk);
    lastblk = exitblk;

    if (always != 0) {
        alwaysblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                  genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, alwaysblk);
        exprgen_gen(gctx, always);
        update_btrack(gctx, bt, expr_genref(always));
        lastblk = alwaysblk;
    }

    if (otherwise != 0) {
        otherwiseblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                     genlabel(gctx));
        LLVMPositionBuilderAtEnd(gctx->builder, otherwiseblk);
        exprgen_gen(gctx, otherwise);
        if (always == 0) {
            update_btrack(gctx, bt, expr_genref(otherwise));
        } else {
            LLVMBuildBr(gctx->builder, alwaysblk);
        }
        lastblk = otherwiseblk;
    }

    numtests = exprseq_length(selseq);
    if (numtests == 0) {
        LLVMPositionBuilderAtEnd(gctx->builder, curblk);
        if (otherwise == 0) {
            if (always == 0) {
                update_btrack(gctx, bt, LLVMConstAllOnes(gctx->fullword_type));
            } else {
                LLVMBuildBr(gctx->builder, alwaysblk);
            }
        } else {
            LLVMBuildBr(gctx->builder, otherwiseblk);
        }
        finalize_btrack(gctx, bt, node);
        return 1;
    }
    st = malloc(numtests * sizeof(struct seltrack_s));
    memset(st, 0, numtests * sizeof(struct seltrack_s));
    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        expr_node_t *subsel;
        if (expr_selector_action(sel) == otherwise && otherwise != 0) {
            st[i].matchdest = otherwiseblk;
        } else {
            st[i].matchdest= LLVMInsertBasicBlockInContext(gctx->llvmctx, lastblk,
                                                          genlabel(gctx));
            LLVMPositionBuilderAtEnd(gctx->builder, st[i].matchdest);
            exprgen_gen(gctx, expr_selector_action(sel));
            st[i].matchdestend = LLVMGetInsertBlock(gctx->builder);
            st[i].matchval = expr_genref(expr_selector_action(sel));
        }
        st[i].testblk = (i == 0
                         ? curblk
                         : LLVMInsertBasicBlockInContext(gctx->llvmctx,
                                                         st[i].matchdest,
                                                         genlabel(gctx)));
        LLVMPositionBuilderAtEnd(gctx->builder, st[i].testblk);
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
            if (st[i].testval == 0) {
                st[i].testval = v;
            } else {
                st[i].testval = LLVMBuildOr(gctx->builder, st[i].testval, v,
                                            gentempname(gctx));
            }
        }
        st[i].testend = LLVMGetInsertBlock(gctx->builder);
        if (sel->tq_next == 0) lastblk = st[i].testblk;
    }

    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        LLVMBasicBlockRef nextifnomatch, nextaftermatchaction;
        LLVMPositionBuilderAtEnd(gctx->builder, st[i].testend);
        if (sel->tq_next == 0) {
            if (otherwise == 0) {
                if (always == 0) {
                    update_btrack_phi(gctx, bt, 0,
                                      LLVMConstAllOnes(gctx->fullword_type));
                    nextifnomatch = exitblk;
                } else {
                    nextifnomatch = alwaysblk;
                }
            } else {
                nextifnomatch = otherwiseblk;
            }
            nextaftermatchaction = (always == 0 ? exitblk : alwaysblk);
        } else {
            nextifnomatch = st[i+1].testblk;
            nextaftermatchaction = (is_selectone ? exitblk : st[i+1].testblk);
        }
        LLVMBuildCondBr(gctx->builder, st[i].testval, st[i].matchdest,
                        nextifnomatch);
        if (nextifnomatch == exitblk) bt->branchcount += 1;
        if (otherwise == 0 || st[i].matchdest != otherwiseblk) {
            LLVMPositionBuilderAtEnd(gctx->builder, st[i].matchdestend);
            if (nextaftermatchaction == exitblk) {
                update_btrack(gctx, bt, st[i].matchval);
            } else if (LLVMGetBasicBlockTerminator(st[i].matchdestend) == 0) {
                LLVMBuildBr(gctx->builder, nextaftermatchaction);
            }
        }
    }
    finalize_btrack(gctx, bt, node);
    free(st);

    return 1;
}
static int
gencode_expr_CTRL_LOOPWU (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v;
    LLVMBasicBlockRef curblk, exitblk, loopblk, testblk;
    branchtrack_t *bt;

    curblk = LLVMGetInsertBlock(gctx->builder);
    exitblk = create_exitblock(gctx, 0);

    bt = new_btrack(gctx, exitblk);

    assert(LLVMGetBasicBlockTerminator(curblk) == 0);
    if (expr_wuloop_type(node) == LOOP_PRETEST) {
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                genlabel(gctx));
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, genlabel(gctx));
        LLVMBuildBr(gctx->builder, testblk);
    } else {
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                                genlabel(gctx));
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, testblk,
                                                genlabel(gctx));
        LLVMBuildBr(gctx->builder, loopblk);
    }
    LLVMPositionBuilderAtEnd(gctx->builder, testblk);
    exprgen_gen(gctx, expr_wuloop_test(node));
    testblk = LLVMGetInsertBlock(gctx->builder);
    v = LLVMBuildTrunc(gctx->builder, expr_genref(expr_wuloop_test(node)),
                       gctx->onebit, gentempname(gctx));
    LLVMBuildCondBr(gctx->builder, v, loopblk, exitblk);
    bt->branchcount += 1;
    update_btrack_phi(gctx, bt, testblk, LLVMConstAllOnes(gctx->fullword_type));
    LLVMPositionBuilderAtEnd(gctx->builder, loopblk);
    bt->next = gctx->curloopbt;
    gctx->curloopbt = bt;
    exprgen_gen(gctx, expr_wuloop_body(node));
    loopblk = LLVMGetInsertBlock(gctx->builder);
    if (LLVMGetBasicBlockTerminator(loopblk) == 0) {
        LLVMBuildBr(gctx->builder, testblk);
    }
    gctx->curloopbt = bt->next;
    finalize_btrack(gctx, bt, node);
    return 1;
}
static int
gencode_expr_CTRL_LOOPID (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef v, testphi, initval, endval, stepval;
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullword_type);
    LLVMValueRef zero = LLVMConstInt(gctx->fullword_type, 0, 0);
    LLVMValueRef pos1 = LLVMConstInt(gctx->fullword_type, 1, 0);
    LLVMBasicBlockRef curblk, exitblk, loopblk, testblk;
    branchtrack_t *bt;
    optype_t cmpop = expr_idloop_cmptype(node);
    gen_datasym_t *lindex = sym_genspace(expr_idloop_index(node));
    int is_decr;

    is_decr = (cmpop == OPER_CMP_GEQ || cmpop == OPER_CMP_GEQU ||
               cmpop == OPER_CMP_GEQA);
    curblk = LLVMGetInsertBlock(gctx->builder);
    exitblk = create_exitblock(gctx, 0);
    bt = new_btrack(gctx, exitblk);

    loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk,
                                            genlabel(gctx));
    testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, genlabel(gctx));
    if (expr_idloop_init(node) != 0) {
        exprgen_gen(gctx, expr_idloop_init(node));
        initval = expr_genref(expr_idloop_init(node));
    } else {
        initval = (is_decr ? neg1 : zero); // neg1 (unsigned) is +Infinity
    }
    if (expr_idloop_term(node) != 0) {
        exprgen_gen(gctx, expr_idloop_term(node));
        endval = expr_genref(expr_idloop_term(node));
    } else {
        endval = (is_decr ? zero : neg1); // neg1 (unsigned) is +Infinity
    }
    if (expr_idloop_step(node) != 0) {
        exprgen_gen(gctx, expr_idloop_step(node));
        stepval = expr_genref(expr_idloop_step(node));
    } else {
        stepval = pos1; // subtracted for DECR, added for INCR
    }
    curblk = LLVMGetInsertBlock(gctx->builder);
    LLVMBuildBr(gctx->builder, testblk);

    LLVMPositionBuilderAtEnd(gctx->builder, testblk);
    testphi = LLVMBuildPhi(gctx->builder, gctx->fullword_type, gentempname(gctx));
    LLVMAddIncoming(testphi, &initval, &curblk, 1);
    LLVMBuildStore(gctx->builder, testphi, lindex->value);
    v = LLVMBuildICmp(gctx->builder, pred[cmpop-OPER_CMP_EQL],
                      testphi, endval, gentempname(gctx));
    LLVMBuildCondBr(gctx->builder, v, loopblk, exitblk);
    update_btrack_phi(gctx, bt, testblk, neg1);
    bt->branchcount += 1;

    bt->next = gctx->curloopbt;
    gctx->curloopbt = bt;
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
    gctx->curloopbt = bt->next;
    finalize_btrack(gctx, bt, node);

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
    LLVMBasicBlockRef entryblk, exitblk;

    if (gctx->builder != 0) {
        int i = gctx->bstkidx;
        assert(gctx->bstkidx < BSTKSIZE);
        gctx->stack[i].curbldpos = LLVMGetInsertBlock(gctx->builder);
        gctx->stack[i].builder = gctx->builder;
        gctx->stack[i].tmpidx = gctx->tmpidx;
        gctx->stack[i].lblidx = gctx->lblidx;
        gctx->stack[i].curfn  = gctx->curfn;
        gctx->stack[i].curfnbt = gctx->curfnbt;
        gctx->stack[i].curloopbt = gctx->curloopbt;
        gctx->stack[i].curblkbt = gctx->curblkbt;
        gctx->bstkidx += 1;
    }
    gctx->tmpidx = 0;
    gctx->lblidx = 0;
    gctx->curloopbt = gctx->curblkbt = 0;
    entryblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                             genlabel(gctx));
    gctx->builder = LLVMCreateBuilderInContext(gctx->llvmctx);
    LLVMPositionBuilderAtEnd(gctx->builder, entryblk);
    exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                            genlabel(gctx));
    gctx->curfnbt = new_btrack(gctx, exitblk);
    gctx->curfn = thisfn;

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

    gctx->curfn = thisfn;
    LLVMPositionBuilderAtEnd(gctx->builder, LLVMGetEntryBasicBlock(thisfn));
    exprgen_gen(gctx, rtnexpr);
    update_btrack(gctx, gctx->curfnbt, ((attr->flags & SYM_M_NOVALUE) == 0
                                        ? expr_genref(rtnexpr) : 0));
    finalize_btrack(gctx, gctx->curfnbt, rtnexpr);
    if (attr->flags & SYM_M_NOVALUE) {
        LLVMBuildRetVoid(gctx->builder);
    } else {
        LLVMValueRef retval = expr_genref(rtnexpr);
        if (retval == 0) retval = LLVMConstInt(gctx->fullword_type, 0, 0);
        LLVMBuildRet(gctx->builder, retval);
    }
    LLVMDisposeBuilder(gctx->builder);
    LLVMVerifyFunction(thisfn, LLVMPrintMessageAction);
    LLVMRunFunctionPassManager(gctx->passmgr, thisfn);
    if (gctx->bstkidx > 0) {
        int i = --gctx->bstkidx;
        gctx->builder = gctx->stack[i].builder;
        gctx->tmpidx = gctx->stack[i].tmpidx;
        gctx->lblidx = gctx->stack[i].lblidx;
        gctx->curfn = gctx->stack[i].curfn;
        gctx->curfnbt = gctx->stack[i].curfnbt;
        gctx->curloopbt = gctx->stack[i].curloopbt;
        gctx->curblkbt = gctx->stack[i].curblkbt;
        LLVMPositionBuilderAtEnd(gctx->builder, gctx->stack[i].curbldpos);
    } else {
        gctx->builder = 0;
        gctx->curfn = 0;
        gctx->curfnbt = gctx->curloopbt = gctx->curblkbt = 0;
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

static LLVMValueRef
gen_initializer_llvmconst (gencodectx_t gctx, initval_t *iv,
                           unsigned int padcount,
                           LLVMTypeRef *ctype)
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
                varr[i] = gen_initializer_llvmconst(gctx, iv->data.listptr, 0, &t);
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
        v = varr[0];
        *ctype = LLVMTypeOf(v);
        free(varr);
        return v;
    }

    c = LLVMConstStructInContext(gctx->llvmctx, varr, count, 1);
    v = LLVMAddGlobal(gctx->module, LLVMTypeOf(c), genglobname(gctx));
    LLVMSetLinkage(v, LLVMPrivateLinkage);
    LLVMSetInitializer(v, c);
    *ctype = LLVMTypeOf(v);
    return v;

}


static LLVMValueRef
gen_initialized_datasym (gencodectx_t gctx, name_t *np, gen_datasym_t *gd,
                         data_attr_t *attr)
{
    LLVMValueRef dval, cval;
    LLVMTypeRef dtype;

    cval = gen_initializer_llvmconst(gctx, attr->ivlist, 0, &dtype);
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
        cval = gen_initializer_llvmconst(gctx, attr->ivlist, padcount, &ctype);
        if (ctype != byteptr) {
            LLVMTypeKind ctk = LLVMGetTypeKind(ctype);
            if (ctk == LLVMPointerTypeKind) {
                cval = LLVMBuildPointerCast(gctx->builder, cval,
                                            byteptr, gentempname(gctx));
            } else {
                cval = LLVMBuildIntToPtr(gctx->builder, cval,
                                         byteptr, gentempname(gctx));
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
        gd->is_local = 1;
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
            // XXX This doesn't seem right, but appears to be needed
            //     or LLVM complains about setting internal linkage.
            LLVMSetInitializer(gd->value, LLVMConstNull(gd->gentype));
        }
        if (attr->owner != 0) {
            LLVMSetSection(gd->value, llvm_section_for_psect(attr->owner,
                                                             attr->ivlist != 0,
                                                             attr->sc == SYMSCOPE_EXTERNAL));
        }
        if (name_globalname(namectx, np) == 0) {
            LLVMSetLinkage(gd->value, LLVMInternalLinkage);
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

    gr->returntype = ((attr->flags & SYM_M_NOVALUE) != 0
                      ? gctx->novalue_type
                      : gctx->fullword_type);

    extantfn = LLVMGetNamedFunction(gctx->module, namestr);
    
    if (attr->sc == SYMSCOPE_EXTERNAL || (attr->flags & SYM_M_FORWARD) != 0) {
        if (extantfn != 0) return 1;
        gr->gentype = LLVMFunctionType(gr->returntype, 0, 0, 1);
        gr->genfn = LLVMAddFunction(gctx->module, namestr, gr->gentype);
        return 1;
    }
    argtypes = build_argtypes(gctx, &attr->inargs);
    gr->gentype = LLVMFunctionType(gr->returntype, argtypes,
                                   namereflist_length(&attr->inargs), 0);
    if (gr->gentype == 0) {
        return 0;
    }
    if (extantfn != 0) {
        LLVMSetValueName(extantfn, "to.be.deleted");
    }
    thisfn = LLVMAddFunction(gctx->module, namestr, gr->gentype);
    if (extantfn != 0) {
        LLVMValueRef castfn;
        LLVMTypeRef extype = LLVMTypeOf(extantfn);
        if (gr->gentype == extype) {
            LLVMReplaceAllUsesWith(extantfn, thisfn);
        } else {
            castfn = LLVMConstBitCast(thisfn, extype);
            LLVMReplaceAllUsesWith(extantfn, castfn);
        }
        LLVMDeleteFunction(extantfn);
    }
    if (name_globalname(expr_namectx(gctx->ectx), np) != 0) {
        LLVMSetLinkage(thisfn, LLVMExternalLinkage);
    } else {
        LLVMSetLinkage(thisfn, LLVMInternalLinkage);
    }
    if (attr->owner != 0) {
        LLVMSetSection(thisfn, llvm_section_for_psect(attr->owner, 0, 0));
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
    char *dl;

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

    LLVMSetTarget(gctx->module, LLVMGetTargetMachineTriple(gctx->target_machine));
    dl = LLVMCopyStringRepOfTargetData(LLVMGetTargetMachineData(gctx->target_machine));
    LLVMSetDataLayout(gctx->module, dl);
    LLVMDisposeMessage(dl);

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
    char *err;

    if (np != gctx->modnp || gm->module != gctx->module) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "gencode_module_end");
    }

    LLVMVerifyModule(gctx->module, LLVMPrintMessageAction, 0);
    LLVMDumpModule(gctx->module);
    err = 0;
    if (LLVMTargetMachineEmitToFile(gctx->target_machine, gctx->module, "blah.s",
                                    LLVMAssemblyFile, &err)) {
        if (err) { fprintf(stderr, "%s\n", err); LLVMDisposeMessage(err); }
    }
    LLVMDisposePassManager(gctx->passmgr);
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
    LLVMTargetRef target;
    char default_triple[64], *err;

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

    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();

    err = 0;
    strcpy(default_triple, HelperGetDefaultTriple());
    target = HelperLookupTarget(default_triple, &err);
    if (target == 0) { if (err != 0) { fprintf(stderr, "%s\n", err); free(err); }}
    gctx->target_machine = LLVMCreateTargetMachine(target, default_triple, "", "", LLVMCodeGenLevelNone, LLVMRelocPIC, LLVMCodeModelDefault);
    HelperSetAsmVerbosity(gctx->target_machine, 1);

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

