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
#include <stdlib.h>

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
    stgctx_t            stg;
    machinedef_t        *mach;
    name_t              *modnp;
    unsigned int        tmpidx;
    unsigned int        lblidx;
    LLVMContextRef      llvmctx;
    LLVMModuleRef       module;
    LLVMTypeRef         novalue_type;
    LLVMTypeRef         fullword_type;
    LLVMTypeRef         fullword_pointer;
    LLVMTypeRef         unit_type;
    LLVMTypeRef         unit_pointer;
    LLVMValueRef        curfn;
    LLVMValueRef        curfnphi;
    LLVMBasicBlockRef   curfnexit;
    LLVMValueRef        curblkphi;
    LLVMBasicBlockRef   curblkexit;
    LLVMBuilderRef      builder;
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
static void
namestring_from_dsc (char *buf, strdesc_t *dsc)
{
    size_t len = (dsc->len > NAME_SIZE-1 ? NAME_SIZE-1 : dsc->len);
    memcpy(buf, dsc->ptr, len);
    buf[len] = '\0';
}

LLVMTypeRef gendatatype (gencodectx_t gctx, data_attr_t *attr, seg_t *seg)
{
    machinedef_t *mach = gctx->mach;
    unsigned int units;
    
    if (attr->flags & SYM_M_REF) {
        return LLVMPointerType(gctx->fullword_type, 0);
    }
    units = (seg == 0 ? attr->units : (unsigned int) seg_size(seg));
    if (units == 1) return gctx->unit_type;
    if (units == 0 || units == machine_scalar_units(mach)) {
        return gctx->fullword_type;
    }
    if (units < machine_scalar_units(mach) && units != 0) {
        return LLVMIntTypeInContext(gctx->llvmctx,
                                    units * machine_unit_bits(mach));
    }
    
    return LLVMArrayType(gctx->unit_type, units);
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
        argtypes[i] = gendatatype(gctx, attr, 0);
    }
    
    return argtypes;
}

void
set_argnames (gencodectx_t gctx, LLVMValueRef thisfn, namereflist_t *argrefs)
{
    LLVMValueRef *args;
    char namestr[NAME_SIZE];
    nameref_t *arg;
    int i;
    
    if (namereflist_length(argrefs) == 0) {
        return;
    }
    args = malloc(namereflist_length(argrefs)*sizeof(LLVMValueRef));
    LLVMGetParams(thisfn, args);
    for (i = 0, arg = namereflist_head(argrefs);
         i < namereflist_length(argrefs); i++, arg = arg->tq_next) {
        namestring_from_dsc(namestr, name_string(arg->np));
        LLVMSetValueName(args[i], namestr);
        datasym_genref_set(arg->np, args[i]);
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
    unsigned int nbits = machine_scalar_bits(gctx->mach);
    LLVMValueRef val;
    LLVMTypeRef mytype;

    switch (name_type(np)) {
        case LEXTYPE_NAME_DATA: {
            data_attr_t *attr = datasym_attr(np);
            unsigned int units;
            val = datasym_genref(np);
            if (attr->flags & SYM_M_ARG) {
                expr_genref_set(node, val);
                return 1;
            }
            units = (unsigned int)seg_size(datasym_seg(np));
            if (units < machine_scalar_units(gctx->mach)) {
                nbits = units * machine_unit_bits(gctx->mach);
            }
            break;
        }
        case LEXTYPE_NAME_ROUTINE:
            val = rtnsym_genref(np);
            break;
        default:
            expr_signal(gctx->ectx, STC__INTCMPERR,
                        "gencode_expr_PRIM_SEG");
            return 0;
            break;
    }
    mytype = (nbits == machine_scalar_bits(gctx->mach) ? gctx->fullword_pointer
              : LLVMPointerType(LLVMIntTypeInContext(gctx->llvmctx, nbits), 0));
    if (offset != 0) {
        LLVMValueRef off = LLVMConstInt(gctx->fullword_type, offset, 1);
        val = LLVMBuildCast(gctx->builder, LLVMPtrToInt, val,
                            gctx->fullword_type, gentempname(gctx));
        val = LLVMBuildAdd(gctx->builder, val, off, gentempname(gctx));
        val = LLVMBuildCast(gctx->builder, LLVMIntToPtr,
                            val, mytype, gentempname(gctx));
    } else {
        val = LLVMBuildPointerCast(gctx->builder, val, mytype, gentempname(gctx));

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
    return 1;
}
static int
gencode_expr_PRIM_STRUREF (gencodectx_t gctx, expr_node_t *node)
{
    LLVMValueRef val;
    
    exprgen_gen(gctx, expr_struref_accexpr(node));
    val = expr_genref(expr_struref_accexpr(node));
    val = LLVMBuildPointerCast(gctx->builder, val,
                               gctx->fullword_pointer,
                               gentempname(gctx));
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
    char namestr[NAME_SIZE];
    LLVMBasicBlockRef cbesave;
    LLVMValueRef cbpsave, normalval;
    
    if (namereflist_length(labels) > 0) {
        lbl = namereflist_head(labels);
        cbesave = gctx->curblkexit;
        namestring_from_dsc(namestr, name_string(lbl->np));
        gctx->curblkexit = LLVMAppendBasicBlockInContext(gctx->llvmctx,
                                                         gctx->curfn,
                                                         namestr);
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
            label_genref_set(lbl->np, gctx->curblkphi);
            label_exitpoint_set(lbl->np, gctx->curblkexit);
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
            if (expr_type(base) == EXPTYPE_PRIM_SEG) {
                data_attr_t *attr = datasym_attr(expr_seg_name(base));
                if (attr->flags & SYM_M_ARG) {
                    v = rhs;
                    break;
                }
                v = LLVMBuildLoad(gctx->builder, rhs, valname);
                if (seg_size(datasym_seg(expr_seg_name(base))) <
                    machine_scalar_units(gctx->mach)) {
                    if (attr->flags & SYM_M_SIGNEXT) {
                        v = LLVMBuildSExt(gctx->builder, v, gctx->fullword_type, gentempname(gctx));
                    } else {
                        v = LLVMBuildZExt(gctx->builder, v, gctx->fullword_type, gentempname(gctx));
                    }
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
        case OPER_ASSIGN:
            if (expr_type(elhs) == EXPTYPE_PRIM_FLDREF) {
                LLVMValueRef neg1, srcmask, dstmask, rs;
                neg1 = LLVMConstAllOnes(gctx->fullword_type);
                v = LLVMBuildShl(gctx->builder, neg1,
                                       expr_genref(expr_fldref_size(elhs)),
                                       gentempname(gctx));
                srcmask = LLVMBuildNot(gctx->builder, v, gentempname(gctx));
                v = LLVMBuildShl(gctx->builder, srcmask,
                                 expr_genref(expr_fldref_pos(elhs)),
                                 gentempname(gctx));
                dstmask = LLVMBuildNot(gctx->builder, v, gentempname(gctx));
                v = LLVMBuildLoad(gctx->builder, lhs, gentempname(gctx));
                v = LLVMBuildAnd(gctx->builder, v, dstmask, gentempname(gctx));
                rs = LLVMBuildAnd(gctx->builder, rhs, srcmask, gentempname(gctx));
                rs = LLVMBuildShl(gctx->builder, rs, expr_genref(expr_fldref_pos(elhs)), gentempname(gctx));
                v = LLVMBuildOr(gctx->builder, v, rs, gentempname(gctx));
                LLVMBuildStore(gctx->builder, v, lhs);
            } else {
                v = rhs;
                if (expr_type(elhs) == EXPTYPE_PRIM_SEG) {
                    seg_t *seg = datasym_seg(expr_seg_name(elhs));
                    if (seg_size(seg) != machine_scalar_units(gctx->mach)) {
                        LLVMTypeRef dtype;
                        dtype = LLVMIntTypeInContext(gctx->llvmctx,
                                                     (unsigned int)seg_size(seg) * machine_unit_bits(gctx->mach));
                        v = LLVMBuildTrunc(gctx->builder, rhs, dtype,
                                           gentempname(gctx));
                    }
                }
                LLVMBuildStore(gctx->builder, v, lhs);
            }
            v = rhs;
            break;
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
            v = LLVMBuildICmp(gctx->builder, pred[op-LEXTYPE_OP_EQL],
                              lhs, rhs, valname);
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
            // XXX
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
        LLVMBasicBlockRef gotoblk = label_exitpoint(np);
        if (exitexp != 0) {
            LLVMValueRef phi = label_genref(np);
            LLVMBasicBlockRef here = LLVMGetInsertBlock(gctx->builder);
            LLVMAddIncoming(phi, &exitval, &here, 1);
        }
        LLVMBuildBr(gctx->builder, gotoblk);
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
    return 1;
}
static int
gencode_expr_CTRL_LOOPID (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
// --- Begin API ---

/*
 * gencode_init
 *
 * Module initialization.
 */
gencodectx_t
gencode_init (logctx_t logctx, machinedef_t *mach, stgctx_t stg)
{
    gencodectx_t gctx = malloc(sizeof(struct gencodectx_s));

    if (gctx == 0) return 0;
    memset(gctx, 0, sizeof(struct gencodectx_s));
    gctx->stg = stg;
    gctx->mach = mach;
    gctx->llvmctx = LLVMContextCreate();
    if (gctx->llvmctx == 0) {
        log_signal(logctx, 0, STC__INTCMPERR, "gencode_init");
        free(gctx);
        return 0;
    }

    gctx->novalue_type = LLVMVoidTypeInContext(gctx->llvmctx);
    gctx->fullword_type = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(mach));
    gctx->fullword_pointer = LLVMPointerType(gctx->fullword_type, 0);
    if (machine_scalar_units(mach) == 1) {
        gctx->unit_type = gctx->fullword_type;
        gctx->unit_pointer = gctx->fullword_pointer;
    } else {
        gctx->unit_type = LLVMIntTypeInContext(gctx->llvmctx, machine_unit_bits(mach));
        gctx->unit_pointer = LLVMPointerType(gctx->unit_type, 0);
    }

    return gctx;
    
} /* gencode_init */

/*
 * gencode_routine_begin
 */
int
gencode_routine_begin (gencodectx_t gctx, name_t *np)
{
    LLVMValueRef thisfn = rtnsym_genref(np);
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
    LLVMValueRef thisfn = rtnsym_genref(np);
    expr_node_t *rtnexpr = rtnsym_expr(np);
    LLVMBasicBlockRef lastinfn, prevbb;
    LLVMValueRef exprval;
    
    gctx->curfn = thisfn;
    LLVMPositionBuilderAtEnd(gctx->builder, LLVMGetEntryBasicBlock(thisfn));
    exprgen_gen(gctx, rtnexpr);
    exprval = expr_genref(rtnexpr);
    lastinfn = LLVMGetLastBasicBlock(thisfn);
    prevbb = LLVMGetPreviousBasicBlock(lastinfn);
    LLVMAddIncoming(gctx->curfnphi, &exprval, &prevbb, 1);
    LLVMBuildBr(gctx->builder, gctx->curfnexit);
    LLVMDisposeBuilder(gctx->builder);
    gctx->builder = 0;
    gctx->curfn = 0;
    gctx->curfnphi = 0;
    return 1;
}

/*
 * gencode_litsym
 */
int
gencode_litsym (gencodectx_t gctx, name_t *np)
{
    literal_attr_t *attr = litsym_attr(np);
    namectx_t namectx = scope_namectx(name_scope(np));
    char namestr[NAME_SIZE];
    name_t *gnp;
    LLVMTypeRef mytype;
    LLVMValueRef val;

    namestring_from_dsc(namestr, name_string(np));

    gnp = name_globalname(namectx, np);
    if (gnp == 0 || litsym_genref(gnp) == 0) {
        mytype = (attr->width == machine_scalar_bits(gctx->mach))
                 ? gctx->fullword_type
                 : LLVMIntTypeInContext(gctx->llvmctx, attr->width);
        val = LLVMConstInt(mytype, name_value_unsigned(np), (attr->flags & SYM_M_SIGNEXT) != 0);
        litsym_genref_set(np, val);
        if (gnp != 0) {
            LLVMValueRef gval;
            gval = LLVMAddGlobal(gctx->module, mytype, namestr);
            LLVMSetInitializer(gval, val);
            LLVMSetGlobalConstant(gval, 1);
            litsym_genref_set(gnp, gval);
        }
    } else {
        litsym_genref_set(np, litsym_genref(gnp));
    }

    return 1;

} /* gencode_litsym */


/*
 * gencode_datasym
 */
int
gencode_datasym (gencodectx_t gctx, name_t *np)
{
    namectx_t namectx = scope_namectx(name_scope(np));
    data_attr_t *attr = datasym_attr(np);
    seg_t *seg = datasym_seg(np);
    char namestr[NAME_SIZE];
    LLVMTypeRef mytype;
    LLVMValueRef val;

    if ((attr->flags & SYM_M_FORWARD) || seg == 0) {
        return 1;
    }
    mytype = gendatatype(gctx, attr, seg);
    namestring_from_dsc(namestr, name_string(np));
    if (seg != 0 && seg_type(seg) == SEGTYPE_STACK) {
        val = LLVMBuildAlloca(gctx->builder, mytype, namestr);
    } else if (seg == 0 || seg_type(seg) == SEGTYPE_STATIC) {
        val = LLVMAddGlobal(gctx->module, mytype, namestr);
        if (name_globalname(namectx, np) == 0) {
            LLVMSetLinkage(val, LLVMInternalLinkage);
        }
    }
    datasym_genref_set(np, val);

    return 1;
    
}

/*
 * gencode_rtnsym
 */
int
gencode_rtnsym (gencodectx_t gctx, name_t *rnp)
{
    routine_attr_t *attr = rtnsym_attr(rnp);
    char namestr[NAME_SIZE];
    LLVMTypeRef *argtypes;
    LLVMTypeRef thisfntype;
    LLVMValueRef thisfn;

    if (attr->flags & SYM_M_FORWARD) {
        return 1;
    }
    namestring_from_dsc(namestr, name_string(rnp));
    argtypes = build_argtypes(gctx, &attr->inargs);
    thisfntype = LLVMFunctionType((attr->flags & SYM_M_NOVALUE)
                                  ? gctx->novalue_type
                                  : gctx->fullword_type, argtypes,
                                  namereflist_length(&attr->inargs), 1);
    if (thisfntype == 0) {
        return 0;
    }
    thisfn = LLVMGetNamedFunction(gctx->module, namestr);
    if (thisfn == 0) {
        thisfn = LLVMAddFunction(gctx->module, namestr, thisfntype);
        if (name_globalname(expr_namectx(gctx->ectx), rnp) != 0) {
            LLVMSetLinkage(thisfn, LLVMExternalLinkage);
        } else {
            LLVMSetLinkage(thisfn, LLVMInternalLinkage);
        }
    }
    set_argnames(gctx, thisfn, &attr->inargs);
    rtnsym_genref_set(rnp, thisfn);

    return 1;

} /* gencode_rtnsym */

/*
 * gencode_module_begin
 */
int
gencode_module_begin (gencodectx_t gctx, void *exprctx, name_t *modnp)
{
    char modname[NAME_SIZE];

    gctx->modnp = modnp;
    gctx->ectx = exprctx;
    namestring_from_dsc(modname, name_string(modnp));
    gctx->module = LLVMModuleCreateWithNameInContext(modname, gctx->llvmctx);

    return (gctx->module != 0);

} /* gencode_module_begin */

int
gencode_module_end (gencodectx_t gctx, name_t *np)
{
    if (np != gctx->modnp) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "gencode_module_end");
    }
    
    LLVMDumpModule(gctx->module);
    LLVMDisposeModule(gctx->module);

    return 1;

} /* gencode_module_end */

