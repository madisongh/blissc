/*
 *++
 *	File:			llvm_expgen.c
 *
 *	Abstract:		Expression generation for LLVM back-end
 *
 *  Module description:
 *
 *       This module generates LLVM IR for expressions.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		19-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */

#include "llvmgen.h"


LLVMValueRef
llvmgen_expression (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    exprtype_t type = expr_type(exp);

    if (gctx->expgen_funcs[type] == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_expression");
    }

    return (*gctx->expgen_funcs[type])(gctx, exp, neededtype);

} /* llvmgen_expression */

LLVMValueRef
llvmgen_addr_expression (gencodectx_t gctx, expr_node_t *exp, unsigned int *flagsp)
{
    exprtype_t type = expr_type(exp);

    if (type == EXPTYPE_PRIM_SEG) {
        return llvmgen_segaddress(gctx, expr_seg_name(exp), 0, flagsp);
    }

    if (type == EXPTYPE_PRIM_FLDREF) {
        return llvmgen_addr_expression(gctx, expr_fldref_addr(exp), flagsp);
    }

    if (flagsp != 0) *flagsp = 0;
    return llvmgen_expression(gctx, exp, gctx->unitptrtype);

} /* llvmgen_addr_expression */

LLVMValueRef
gen_literal (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    strdesc_t *str = expr_litstring(exp);
    LLVMValueRef result;

    if (str == 0) {
        int signext = machine_signext_supported(gctx->mach);
        result = LLVMConstInt(gctx->fullwordtype, expr_litval(exp), signext);
    } else if (str->len <= machine_scalar_units(gctx->mach)) {
        unsigned long ival = 0;
        unsigned int i;
        for (i = str->len; i > 0; i--) ival = (ival << 8) | str->ptr[i];
        result = LLVMConstInt(gctx->fullwordtype, ival, 0);
    } else {
        result = LLVMConstStringInContext(gctx->llvmctx, str->ptr, str->len, 1);
    }

    if (neededtype != 0) result = llvmgen_adjustval(gctx, result, neededtype);

    return result;

} /* gen_literal */

/*
 * gen_fieldref
 *
 * The fetch and assignment operator expression code handle the special cases
 * where we have to apply the field parameters (position, size, sign extension);
 * in all other cases where a field reference appears, its value is simply the
 * address; the field parameters are ignored.
 */
LLVMValueRef
gen_fieldref (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMValueRef result = llvmgen_addr_expression(gctx, expr_fldref_addr(exp), 0);

    if (neededtype != 0) result = llvmgen_adjustval(gctx, result, neededtype);

    return result;

} /* gen_fieldref */

/*
 * gen_struref
 *
 * The PRIM_STRUREF expression type is really just a way to automatically
 * parenthesize a structure-reference expression, so it's treated as a unit
 * by the expression code.  No special handling required here.
 */
LLVMValueRef
gen_struref (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    return llvmgen_expression(gctx, expr_struref_accexpr(exp), neededtype);

} /* gen_struref */

LLVMValueRef
gen_block (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    exprseq_t *seq = expr_blk_seq(exp);
    namereflist_t *labels = expr_blk_labels(exp);
    nameref_t *lbl;
    expr_node_t *e;
    llvm_btrack_t *bt;
    LLVMValueRef val = 0;

    bt = 0;
    if (namereflist_length(labels) > 0) {
        LLVMBasicBlockRef exitblk;
        lbl = namereflist_head(labels);
        exitblk = llvmgen_exitblock_create(gctx, name_azstring(lbl->np));
        bt = llvmgen_btrack_create(gctx, exitblk);
        while (lbl != 0) {
            llvmgen_label_btrack_set(lbl->np, bt);
            lbl = lbl->tq_next;
        }
        bt->next = gctx->curfn->btrack[LLVMGEN_K_BT_BLK];
        gctx->curfn->btrack[LLVMGEN_K_BT_BLK] = bt;
    }

    for (e = exprseq_head(seq); e != 0; e = e->tq_next) {
        val = llvmgen_expression(gctx, e, 0);
        if (bt != 0) llvmgen_btrack_update(gctx, bt, val);
    }

    if (bt != 0) {
        gctx->curfn->btrack[LLVMGEN_K_BT_BLK] = bt->next;
        bt->next = 0;
        val = llvmgen_btrack_finalize(gctx, bt);
        for (lbl = namereflist_head(labels); lbl != 0; lbl = lbl->tq_next) {
            llvmgen_label_btrack_set(lbl->np, 0);
        }
    }

    if (val == 0) {
        if (neededtype != 0) {
            log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
            val = LLVMConstNull(neededtype);
        }
    } else {
        if (neededtype) val = llvmgen_adjustval(gctx, val, neededtype);
    }

    return val;
    
} /* gen_block */

void
llvmgen_expgen_register (gencodectx_t gctx, exprtype_t type, llvmgen_expgen_fn func)
{
    gctx->expgen_funcs[type] = func;

} /* llvmgen_expgen_register */

void
llvmgen_expgen_init (gencodectx_t gctx)
{
    unsigned int bpunit = machine_unit_bits(gctx->mach);
    unsigned int bpval = machine_scalar_bits(gctx->mach);

    gctx->unitptrtype = LLVMPointerType(LLVMIntTypeInContext(gctx->llvmctx, bpunit), 0);
    gctx->fullwordtype = LLVMIntTypeInContext(gctx->llvmctx, bpval);

    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_LIT, gen_literal);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_FLDREF, gen_fieldref);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_STRUREF, gen_struref);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_BLK, gen_block);
    llvmgen_opexpgen_init(gctx);
    llvmgen_ctrlexpgen_init(gctx);
    llvmgen_execfuncgen_init(gctx);

} /* llvmgen_expgen_init */
