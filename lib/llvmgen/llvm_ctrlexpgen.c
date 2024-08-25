/*
 *++
 * llvm_ctrlexpgen.c - Control expression generation for LLVM back-end
 *
 *
 * This module generates LLVM IR for control expressions.
 *
 * Copyright © 2013-2020, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "llvmgen.h"

/*
 * gen_conditional
 *
 * Generates code for a conditional (IF-THEN-ELSE)
 * expression.
 */
static LLVMValueRef
gen_conditional (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    int hasval = expr_has_value(exp);
    LLVMBasicBlockRef consblk, altblk;
    LLVMValueRef consval, altval, test, result;
    LLVMTypeRef resulttype;

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
    resulttype = (hasval ? neededtype : 0);
    consval = llvmgen_expression(gctx, expr_cond_consequent(exp), resulttype);
    if (hasval && resulttype == 0 && consval != 0) {
        resulttype = LLVMTypeOf(consval);
    }
    llvmgen_btrack_update(gctx, bt, (hasval ? consval : 0));
    if (altblk != 0) {
        LLVMPositionBuilderAtEnd(builder, altblk);
        altval = llvmgen_expression(gctx, expr_cond_alternative(exp), resulttype);
        llvmgen_btrack_update(gctx, bt, altval);
    }
    result = llvmgen_btrack_finalize(gctx, bt, resulttype);
    if (neededtype != 0 && result == 0) {
        log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
        result = LLVMConstNull(neededtype);
    }

    return result;

} /* gen_conditional */

/*
 * gen_case
 *
 * Generates a CASE expression.
 */
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

    // If we have a reasonable number of case actions,
    // just use the local buffer to hold them.
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
        actval = llvmgen_expression(gctx, actions[i], (hasval ? gctx->fullwordtype : 0));
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
    result = llvmgen_btrack_finalize(gctx, bt, gctx->fullwordtype);

    if (bbvec != localvec) {
        free(bbvec);
    }

    if (result == 0) {
        if (neededtype != 0) {
            log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__EXPRVALRQ);
            return LLVMConstNull(neededtype);
        }
        return 0;
    }
    
    return llvmgen_adjustval(gctx, result, neededtype, 1);

} /* gen_case */

/*
 * gen_loop_or_block_exit
 *
 * Generates code for LEAVE and EXITLOOP exit expresions.
 */
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

/*
 * gen_return
 *
 * Generates code for RETURN expressions.
 */
static LLVMValueRef
gen_return (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    expr_node_t *retexp = expr_exit_value(exp);
    LLVMValueRef retval = (retexp == 0 ? 0 : llvmgen_expression(gctx, retexp, gctx->fullwordtype));

    llvmgen_btrack_update(gctx, gctx->curfn->btrack[LLVMGEN_K_BT_FUNC], retval);

    if (retval != 0) retval = llvmgen_adjustval(gctx, retval, neededtype, 0);
    return retval;

} /* gen_return */

/*
 * gen_select
 *
 * Generates code for SELECT/SELECTONE expressions.
 */
static LLVMValueRef
gen_select (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    LLVMBuilderRef builder = gctx->curfn->builder;
    optype_t op = expr_sel_cmptype(exp);
    exprseq_t *selseq = expr_sel_selectors(exp);
    unsigned int numtests = exprseq_length(selseq);
    int is_selectone = expr_sel_oneonly(exp);
    int hasval = expr_has_value(exp);
    expr_node_t *sel, *otherwexp;
    LLVMIntPredicate eqlpred, geqpred, leqpred;
    LLVMBasicBlockRef curblk, otherwise, always, insertpoint, testinspoint;
    LLVMValueRef idxval, v, result;
    LLVMTypeRef actiontype = (hasval ? gctx->fullwordtype : 0);
    struct seltrack_s {
        LLVMValueRef testval, matchval;
        LLVMBasicBlockRef testblk, testend;
        LLVMBasicBlockRef matchdest, matchdestend;
    } *st, stlocal[32];
    unsigned int i;

    if (op == OPER_CMP_EQLA) {
        op = (machine_addr_signed(gctx->mach) ? OPER_CMP_EQL : OPER_CMP_EQLU);
    }
    eqlpred = LLVMIntEQ;
    if (op == OPER_CMP_EQL) {
        leqpred = LLVMIntSLE;
        geqpred = LLVMIntSGE;
    } else {
        leqpred = LLVMIntULE;
        geqpred = LLVMIntUGE;
    }
    insertpoint = exitblk;
    idxval = llvmgen_expression(gctx, expr_sel_index(exp), gctx->fullwordtype);
    curblk = LLVMGetInsertBlock(builder);
    if (expr_sel_alwaysaction(exp) != 0) {
        always = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
        LLVMPositionBuilderAtEnd(builder, always);
        v = llvmgen_expression(gctx, expr_sel_alwaysaction(exp), actiontype);
        llvmgen_btrack_update(gctx, bt, v);
        insertpoint = always;
    } else {
        always = 0;
    }

    otherwexp = expr_sel_otherwiseaction(exp);
    if (otherwexp != 0) {
        otherwise = LLVMInsertBasicBlockInContext(gctx->llvmctx, insertpoint,
                                                  llvmgen_label(gctx));
        LLVMPositionBuilderAtEnd(builder, otherwise);
        v = llvmgen_expression(gctx, otherwexp, actiontype);
        if (always == 0) {
            llvmgen_btrack_update(gctx, bt, v);
        } else {
            LLVMBuildBr(builder, always);
        }
        insertpoint = otherwise;
    } else {
        otherwise = 0;
    }

    if (numtests == 0) {
        LLVMPositionBuilderAtEnd(builder, curblk);
        if (otherwise == 0) {
            if (always == 0) {
                llvmgen_btrack_update(gctx, bt,
                                      (hasval ? LLVMConstAllOnes(gctx->fullwordtype) : 0));
            } else {
                LLVMBuildBr(builder, always);
            }
        } else {
            LLVMBuildBr(builder, otherwise);
        }
        result = llvmgen_btrack_finalize(gctx, bt, (hasval ? actiontype : 0));
        return llvmgen_adjustval(gctx, result, neededtype, 1);
    }

    if (numtests > 32) {
        st = malloc(numtests * sizeof(struct seltrack_s));
    } else {
        st = stlocal;
    }
    memset(st, 0, numtests * sizeof(struct seltrack_s));
    testinspoint = insertpoint;
    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        expr_node_t *subsel;
        if (otherwexp != 0 && expr_selector_action(sel) == otherwexp) {
            st[i].matchdest = otherwise;
        } else {
            st[i].matchdest = LLVMInsertBasicBlockInContext(gctx->llvmctx, insertpoint,
                                                            llvmgen_label(gctx));
            LLVMPositionBuilderAtEnd(builder, st[i].matchdest);
            st[i].matchval = llvmgen_expression(gctx, expr_selector_action(sel),
                                                actiontype);
            st[i].matchdestend = LLVMGetInsertBlock(builder);
            if (testinspoint == insertpoint) {
                testinspoint = st[i].matchdest;
            }
        }
        st[i].testblk = (i == 0 ? curblk
                         : LLVMInsertBasicBlockInContext(gctx->llvmctx, testinspoint,
                                                         llvmgen_label(gctx)));
        LLVMPositionBuilderAtEnd(builder, st[i].testblk);
        for (subsel = sel; subsel != 0; subsel = expr_selector_next(subsel)) {
            LLVMValueRef loval, hival;
            loval = llvmgen_expression(gctx, expr_selector_low(subsel), gctx->fullwordtype);
            if (expr_selector_high(subsel) == 0) {
                hival = 0;
            } else {
                hival = llvmgen_expression(gctx, expr_selector_high(subsel), gctx->fullwordtype);
            }
            v = LLVMBuildICmp(builder, (hival == 0 ? eqlpred : geqpred),
                              idxval, loval, llvmgen_temp(gctx));
            if (hival != 0) {
                LLVMValueRef hitest;
                hitest = LLVMBuildICmp(builder, leqpred, idxval, hival, llvmgen_temp(gctx));
                v = LLVMBuildAnd(builder, v, hitest, llvmgen_temp(gctx));
            }
            if (st[i].testval == 0) {
                st[i].testval = v;
            } else {
                st[i].testval = LLVMBuildOr(builder, st[i].testval, v, llvmgen_temp(gctx));
            }
        }
        st[i].testend = LLVMGetInsertBlock(builder);
    }

    for (i = 0, sel = exprseq_head(selseq); sel != 0; i++, sel = sel->tq_next) {
        LLVMBasicBlockRef nextifnomatch, nextaftermatchaction;
        LLVMPositionBuilderAtEnd(builder, st[i].testend);
        if (sel->tq_next == 0) {
            if (otherwise == 0) {
                if (always == 0) {
                    if (hasval) {
                        llvmgen_btrack_update_phi(gctx, bt, 0,
                                                  LLVMConstAllOnes(gctx->fullwordtype));
                    }
                    nextifnomatch = exitblk;
                } else {
                    nextifnomatch = always;
                }
            } else {
                nextifnomatch = otherwise;
            }
            nextaftermatchaction = (always == 0 ? exitblk : always);
        } else {
            nextifnomatch = st[i+1].testblk;
            nextaftermatchaction = (is_selectone ? exitblk : nextifnomatch);
        }
        LLVMBuildCondBr(builder, st[i].testval, st[i].matchdest, nextifnomatch);
        if (nextifnomatch == exitblk) {
            llvmgen_btrack_update_brcount(gctx, bt);
        }
        if (otherwise == 0 || st[i].matchdest != otherwise) {
            LLVMPositionBuilderAtEnd(builder, st[i].matchdestend);
            if (nextaftermatchaction == exitblk) {
                llvmgen_btrack_update(gctx, bt, st[i].matchval);
            } else if (LLVMGetBasicBlockTerminator(st[i].matchdestend) == 0) {
                LLVMBuildBr(builder, nextaftermatchaction);
            }
        }
    }

    result = llvmgen_btrack_finalize(gctx, bt, actiontype);

    if (st != stlocal) {
        free(st);
    }

    return llvmgen_adjustval(gctx, result, neededtype, 1);

} /* gen_select */

/*
 * gen_while_until_loop
 *
 * Generates code for WHILE/UNTIL loops.
 */
static LLVMValueRef
gen_while_until_loop (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMBasicBlockRef loopblk, testblk;
    LLVMValueRef result, val;

    if (expr_wuloop_type(exp) == LOOP_PRETEST_WHILE ||
        expr_wuloop_type(exp) == LOOP_PRETEST_UNTIL) {
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, llvmgen_label(gctx));
        LLVMBuildBr(builder, testblk);
    } else {
        testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
        loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, testblk, llvmgen_label(gctx));
        LLVMBuildBr(builder, loopblk);
    }
    LLVMPositionBuilderAtEnd(builder, testblk);
    val = llvmgen_expression(gctx, expr_wuloop_test(exp), gctx->int1type);
    if (expr_wuloop_type(exp) == LOOP_PRETEST_WHILE ||
        expr_wuloop_type(exp) == LOOP_POSTTEST_WHILE) {
        LLVMBuildCondBr(builder, val, loopblk, exitblk);
    } else {
        LLVMBuildCondBr(builder, val, exitblk, loopblk);
    }
    llvmgen_btrack_update_brcount(gctx, bt);
    llvmgen_btrack_update_phi(gctx, bt, LLVMGetInsertBlock(builder),
                              LLVMConstAllOnes(gctx->fullwordtype));
    LLVMPositionBuilderAtEnd(builder, loopblk);
    bt->next = gctx->curfn->btrack[LLVMGEN_K_BT_LOOP];
    gctx->curfn->btrack[LLVMGEN_K_BT_LOOP] = bt;
    llvmgen_expression(gctx, expr_wuloop_body(exp), 0);
    if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder)) == 0) {
        LLVMBuildBr(builder, testblk);
    }
    gctx->curfn->btrack[LLVMGEN_K_BT_LOOP] = bt->next;
    bt->next = 0;
    result = llvmgen_btrack_finalize(gctx, bt, gctx->fullwordtype);

    return llvmgen_adjustval(gctx, result, neededtype, 1);

} /* gen_while_until_loop */

/*
 * gen_incr_decr_loop
 *
 * Generates code for INCR and DECR loops.
 */
static LLVMValueRef
gen_incr_decr_loop (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    LLVMBuilderRef builder = gctx->curfn->builder;
    optype_t compareop = expr_idloop_cmptype(exp);
    LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullwordtype);
    LLVMValueRef zero = LLVMConstNull(gctx->fullwordtype);
    LLVMValueRef loopidx = llvmgen_segaddress(gctx, expr_idloop_index(exp), 0, 0, 0);
    int addrsigned = machine_addr_signed(gctx->mach);
    LLVMValueRef initval, endval, stepval, testphi, cmpval, result;
    LLVMBasicBlockRef loopblk, testblk, curblk;
    LLVMIntPredicate pred;
    int is_decr;

    is_decr = (compareop == OPER_CMP_GEQ || compareop == OPER_CMP_GEQU
               || compareop == OPER_CMP_GEQA);
    loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    testblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, loopblk, llvmgen_label(gctx));
    if (expr_idloop_init(exp) != 0) {
        initval = llvmgen_expression(gctx, expr_idloop_init(exp), gctx->fullwordtype);
    } else {
        initval = (is_decr ? neg1 : zero); // -1 (unsigned) is +Infinity
    }
    if (expr_idloop_term(exp) != 0) {
        endval = llvmgen_expression(gctx, expr_idloop_term(exp), gctx->fullwordtype);
    } else {
        endval = (is_decr ? zero : neg1);
    }
    if (expr_idloop_step(exp) != 0) {
        stepval = llvmgen_expression(gctx, expr_idloop_step(exp), gctx->fullwordtype);
    } else {
        stepval = LLVMConstInt(gctx->fullwordtype, 1, 0);  // subtracted for DECR
    }
    curblk = LLVMGetInsertBlock(builder);
    LLVMBuildBr(builder, testblk);

    LLVMPositionBuilderAtEnd(builder, testblk);
    testphi = LLVMBuildPhi(builder, gctx->fullwordtype, llvmgen_temp(gctx));
    LLVMAddIncoming(testphi, &initval, &curblk, 1);
    LLVMBuildStore(builder, testphi, loopidx);
    if (!llvmgen_predfromop(compareop, addrsigned, &pred)) {
        log_signal(expr_logctx(gctx->ectx), expr_textpos(exp), STC__INTCMPERR, __func__);
    }
    cmpval = LLVMBuildICmp(builder, pred, testphi, endval, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, cmpval, loopblk, exitblk);
    llvmgen_btrack_update_phi(gctx, bt, LLVMGetInsertBlock(builder), neg1);
    llvmgen_btrack_update_brcount(gctx, bt);

    bt->next = gctx->curfn->btrack[LLVMGEN_K_BT_LOOP];
    gctx->curfn->btrack[LLVMGEN_K_BT_LOOP] = bt;
    LLVMPositionBuilderAtEnd(builder, loopblk);
    llvmgen_expression(gctx, expr_idloop_body(exp), 0);
    if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(builder)) == 0) {
        LLVMValueRef v;
        v = LLVMBuildLoad2(builder, gctx->fullwordtype, loopidx, llvmgen_temp(gctx));
        if (is_decr) {
            v = LLVMBuildSub(builder, v, stepval, llvmgen_temp(gctx));
        } else {
            v = LLVMBuildAdd(builder, v, stepval, llvmgen_temp(gctx));
        }
        LLVMBuildBr(builder, testblk);
        curblk = LLVMGetInsertBlock(builder);
        LLVMAddIncoming(testphi, &v, &curblk, 1);
    }
    gctx->curfn->btrack[LLVMGEN_K_BT_LOOP] = bt->next;
    bt->next = 0;
    result = llvmgen_btrack_finalize(gctx, bt, gctx->fullwordtype);

    return llvmgen_adjustval(gctx, result, neededtype, 1);

} /* gen_incr_decr_loop */

/*
 * llvmgen_ctrlexpgen_init
 *
 * Initialization routine.  Installs the hooks into the
 * dispatcher for the expression types handled in this 
 * module.
 */
void
llvmgen_ctrlexpgen_init (gencodectx_t gctx)
{

    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_COND, gen_conditional);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_CASE, gen_case);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_EXIT, gen_loop_or_block_exit);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_RET, gen_return);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_SELECT, gen_select);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_LOOPWU, gen_while_until_loop);
    llvmgen_expgen_register(gctx, EXPTYPE_CTRL_LOOPID, gen_incr_decr_loop);

} /* llvmgen_ctrlexpgen_init */
