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


void
llvmgen_expgen_register (gencodectx_t gctx, exprtype_t type, llvmgen_expgen_fn func)
{
    gctx->expgen_funcs[type] = func;

} /* llvmgen_expgen_register */


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
        LLVMValueRef addr = llvmgen_segaddress(gctx, expr_seg_name(exp), 0, flagsp);
        if (expr_seg_offset(exp) != 0) {
            addr = llvmgen_adjustval(gctx, addr, gctx->fullwordtype, 0);
            addr = LLVMBuildAdd(gctx->curfn->builder, addr,
                                LLVMConstInt(gctx->fullwordtype, expr_seg_offset(exp), 0),
                                llvmgen_temp(gctx));
            return llvmgen_adjustval(gctx, addr, gctx->unitptrtype, 0);
        }
        return addr;
    }

    if (type == EXPTYPE_PRIM_FLDREF) {
        return llvmgen_addr_expression(gctx, expr_fldref_addr(exp), flagsp);
    }

    if (flagsp != 0) *flagsp = 0;

    return llvmgen_expression(gctx, exp, gctx->unitptrtype);

} /* llvmgen_addr_expression */

static LLVMValueRef
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

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_literal */

/*
 * gen_seg_or_fieldref
 *
 * The fetch and assignment operator expression code handle the special cases
 * where we have to apply the field parameters (position, size, sign extension);
 * in all other cases where a field reference appears, its value is simply the
 * address; the field parameters are ignored.
 */
static LLVMValueRef
gen_seg_or_fieldref (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    expr_node_t *base = (expr_type(exp) == EXPTYPE_PRIM_FLDREF ? expr_fldref_addr(exp) : exp);
    LLVMValueRef result = llvmgen_addr_expression(gctx, base, 0);

    return llvmgen_adjustval(gctx, result, neededtype, machine_addr_signed(gctx->mach));

} /* gen_fieldref */

/*
 * gen_struref
 *
 * The PRIM_STRUREF expression type is really just a way to automatically
 * parenthesize a structure-reference expression, so it's treated as a unit
 * by the expression code.  No special handling required here. XXX Well, maybe there is
 */
static LLVMValueRef
gen_struref (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMValueRef val;
    name_t *np = expr_struref_referer(exp);
    if (np != 0) llvmgen_deref_push(gctx, np);
    val = llvmgen_expression(gctx, expr_struref_accexpr(exp), neededtype);
    if (np != 0) llvmgen_deref_pop(gctx, np);
    return val;
    
} /* gen_struref */

static LLVMValueRef
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
        if (e->tq_next == 0) {
            val = llvmgen_expression(gctx, e, (expr_has_value(exp) ? gctx->fullwordtype : 0));
            if (bt != 0) llvmgen_btrack_update(gctx, bt, val);
        } else {
            llvmgen_expression(gctx, e, 0);
        }
    }

    if (bt != 0) {
        gctx->curfn->btrack[LLVMGEN_K_BT_BLK] = bt->next;
        bt->next = 0;
        val = llvmgen_btrack_finalize(gctx, bt, gctx->fullwordtype);
        for (lbl = namereflist_head(labels); lbl != 0; lbl = lbl->tq_next) {
            llvmgen_label_btrack_set(lbl->np, 0);
        }
    }

    if (val == 0) {
        if (neededtype != 0) {
            // XXX This may or may not be a problem - if the block is exited with
            //     values via RETURN, LEAVE, or EXITLOOP, and all paths are handled,
            //     then there's no problem.  Keep quiet about this for now; when
            //     I can figure out how to determine definitively that there's a
            //     dangling exit path, add it back.
            //log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
            val = LLVMConstNull(neededtype);
        }
    } else {
        val = llvmgen_adjustval(gctx, val, neededtype, 0);
    }

    return val;
    
} /* gen_block */

static LLVMValueRef
gen_routine_call (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    expr_node_t *rtnexp = expr_rtnaddr(exp);
    exprseq_t *args = expr_rtn_inargs(exp);
    expr_node_t *arg;
    LLVMValueRef rtnadr, result, argvals[LLVMGEN_K_MAXARGS];
    LLVMTypeRef type, rettype, argtypes[LLVMGEN_K_MAXARGS];
    unsigned int i, argcount = exprseq_length(args), formalcount;

    if (expr_type(rtnexp) == EXPTYPE_PRIM_SEG &&
        name_type(expr_seg_name(rtnexp)) == LEXTYPE_NAME_ROUTINE) {
        rtnadr = LLVMGetNamedFunction(gctx->module, name_azstring(expr_seg_name(rtnexp)));
        type = LLVMGetElementType(LLVMTypeOf(rtnadr));
        rettype = LLVMGetReturnType(type);
        formalcount = LLVMCountParamTypes(type);
    } else {
        rettype = gctx->fullwordtype;
        type = LLVMFunctionType(rettype, 0, 0, 1);
        rtnadr = llvmgen_expression(gctx, rtnexp, type);
        formalcount = 0;
    }
    if (argcount > LLVMGEN_K_MAXARGS) {
        log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXCCALPARS);
        argcount = LLVMGEN_K_MAXARGS;
    }
    if (formalcount != 0) LLVMGetParamTypes(type, argtypes);
    for (i = 0, arg = exprseq_head(args); i < argcount && arg != 0; i++, arg = arg->tq_next) {
        if (i >= formalcount) argtypes[i] = 0;
        argvals[i] = llvmgen_expression(gctx, arg, argtypes[i]);
    }
    if (argcount < formalcount) {
        log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__INSFPARS);
        for (i = argcount; i < formalcount; i++) argvals[i] = LLVMConstNull(argtypes[i]);
        argcount = formalcount;
    }

    if (LLVMGetTypeKind(rettype) == LLVMVoidTypeKind) {
        result = 0;
        LLVMBuildCall(gctx->curfn->builder, rtnadr, argvals, argcount, "");
        if (neededtype != 0) {
            log_signal(expr_logctx(gctx->ectx), expr_textpos(rtnexp), STC__EXPRVALRQ);
            result = LLVMConstNull(neededtype);
        }
    } else {
        result = LLVMBuildCall(gctx->curfn->builder, rtnadr, argvals, argcount, llvmgen_temp(gctx));
    }

    return (result == 0 ? 0 : llvmgen_adjustval(gctx, result, neededtype, 0));

} /* gen_routine_call */

static LLVMValueRef
gen_noop (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    return (neededtype == 0 ? 0 : LLVMConstNull(neededtype));
    
} /* gen_noop */

void
llvmgen_expgen_init (gencodectx_t gctx)
{
    unsigned int bpunit = machine_unit_bits(gctx->mach);
    unsigned int bpval = machine_scalar_bits(gctx->mach);

    gctx->unitptrtype = LLVMPointerType(LLVMIntTypeInContext(gctx->llvmctx, bpunit), 0);
    gctx->fullwordtype = LLVMIntTypeInContext(gctx->llvmctx, bpval);

    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_LIT, gen_literal);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_SEG, gen_seg_or_fieldref);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_FLDREF, gen_seg_or_fieldref);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_STRUREF, gen_struref);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_BLK, gen_block);
    llvmgen_expgen_register(gctx, EXPTYPE_PRIM_RTNCALL, gen_routine_call);
    llvmgen_expgen_register(gctx, EXPTYPE_NOOP, gen_noop);
    llvmgen_opexpgen_init(gctx);
    llvmgen_ctrlexpgen_init(gctx);
    llvmgen_execfuncgen_init(gctx);

} /* llvmgen_expgen_init */
