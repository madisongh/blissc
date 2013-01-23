/*
 *++
 *	File:			llvm_ctrlexpgen.c
 *
 *	Abstract:		Control expression generation for LLVM back-end
 *
 *  Module description:
 *
 *       This module generates LLVM IR for control expressions.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		19-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */

#include "llvmgen.h"

static LLVMValueRef
gen_conditional (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    LLVMBasicBlockRef consblk, altblk;
    LLVMValueRef consval, altval, test, result;

    test = llvmgen_expression(gctx, expr_cond_test(exp), gctx->int1type);
    if (expr_cond_alternative(exp) != 0) {
        altblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    } else {
        altblk = 0;
    }
    consblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, (altblk == 0 ? exitblk : altblk),
                                            llvmgen_label(gctx));
    LLVMBuildCondBr(builder, test, consblk, (altblk == 0 ? exitblk : altblk));
    if (altblk == 0) llvmgen_btrack_update_brcount(gctx, bt);
    LLVMPositionBuilderAtEnd(builder, consblk);
    consval = llvmgen_expression(gctx, expr_cond_consequent(exp), neededtype);
    llvmgen_btrack_update(gctx, bt, (altblk == 0 ? 0 : consval));
    if (altblk != 0) {
        LLVMPositionBuilderAtEnd(builder, altblk);
        altval = llvmgen_expression(gctx, expr_cond_alternative(exp),
                                    (neededtype == 0 ? LLVMTypeOf(consval) : neededtype));
        llvmgen_btrack_update(gctx, bt, altval);
    }
    result = llvmgen_btrack_finalize(gctx, bt);
    if (neededtype != 0 && result == 0) {
        log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
        result = LLVMConstNull(neededtype);
    }

    return result;

} /* gen_conditional */

static LLVMValueRef
gen_case (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    expr_node_t **actions = expr_case_actions(exp);
    unsigned int lo = (unsigned int) expr_case_lowbound(exp);
    unsigned int hi = (unsigned int) expr_case_highbound(exp);
    long *cases = expr_case_cases(exp);
    unsigned int casecount = (hi-lo+1);
    unsigned int actioncount = (unsigned int) expr_case_actioncount(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    int hasval = expr_has_value(exp);
    int outrange = (int) expr_case_outrange(exp);
    LLVMBasicBlockRef curblk, *bbvec, localvec[256];
    LLVMValueRef idxval, actval, swval, result;
    unsigned int i;

    if (actioncount <= 256) {
        bbvec = localvec;
    } else {
        bbvec = malloc(actioncount * sizeof(LLVMBasicBlockRef));
    }
    memset(bbvec, 0, actioncount * sizeof(LLVMBasicBlockRef));
    idxval = llvmgen_expression(gctx, expr_case_index(exp), gctx->fullwordtype);
    idxval = LLVMBuildSub(builder, idxval, LLVMConstInt(gctx->fullwordtype, lo, 1),
                          llvmgen_temp(gctx));
    curblk = LLVMGetInsertBlock(builder);
    for (i = 0; i < actioncount; i++) {
        bbvec[i] = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
        LLVMPositionBuilderAtEnd(builder, bbvec[i]);
        actval = llvmgen_expression(gctx, actions[i], gctx->fullwordtype);
        llvmgen_btrack_update(gctx, bt, (hasval ? actval : 0));
    }
    LLVMPositionBuilderAtEnd(builder, curblk);
    swval = LLVMBuildSwitch(builder, idxval, (outrange < 0 ? exitblk : bbvec[outrange]), casecount);
    if (outrange < 0) {
        llvmgen_btrack_update_brcount(gctx, bt);
        if (hasval) {
            llvmgen_btrack_update_phi(gctx, bt, curblk, LLVMConstAllOnes(gctx->fullwordtype));
        }
    }
    for (i = 0; i < casecount; i++) {
        LLVMAddCase(swval, LLVMConstInt(gctx->fullwordtype, i, 0), bbvec[cases[i]]);
    }
    result = llvmgen_btrack_finalize(gctx, bt);
    if (result == 0) {
        if (neededtype != 0) {
            log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
            return LLVMConstNull(neededtype);
        }
        return 0;
    }
    
    return llvmgen_adjustval(gctx, result, neededtype);

} /* gen_case */

static LLVMValueRef
gen_loop_or_block_exit (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    name_t *np = expr_exit_label(exp);
    llvm_btrack_t *bt = (np == 0 ? gctx->curfn->btrack[LLVMGEN_K_BT_LOOP] : llvmgen_label_btrack(np));
    expr_node_t *exitexp = expr_exit_value(exp);
    LLVMValueRef exitval = (exitexp == 0 ? 0 : llvmgen_expression(gctx, exitexp, neededtype));

    llvmgen_btrack_update(gctx, bt, exitval);

    return exitval;

} /* gen_loop_or_block_exit */

static LLVMValueRef
gen_return (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    expr_node_t *retexp = expr_exit_value(exp);
    LLVMValueRef retval = (retexp == 0 ? 0 : llvmgen_expression(gctx, retexp, neededtype));

    llvmgen_btrack_update(gctx, gctx->curfn->btrack[LLVMGEN_K_BT_FUNC], retval);

    return retval;

} /* gen_return */

void
llvmgen_ctrlexpgen_init (gencodectx_t gctx)
{

    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_COND, gen_conditional);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_CASE, gen_case);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_EXIT, gen_loop_or_block_exit);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_RET, gen_return);

} /* llvmgen_ctrlexpgen_init */
