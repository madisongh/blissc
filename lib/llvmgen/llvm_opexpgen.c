/*
 *++
 *	File:			llvm_opexpgen.c
 *
 *	Abstract:		Operator expression generation for LLVM back-end
 *
 *  Module description:
 *
 *       This module generates LLVM IR for operator expressions.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		19-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */
#include "llvmgen.h"
#include "blissc/nametable.h"


/*
 * llvmgen_predfromop
 *
 * Translates our OPER_xxx operator type code, for a comparison
 * operator, into an LLVM predicate code.
 */
LLVMIntPredicate
llvmgen_predfromop (optype_t op, int addrsigned)
{
    static LLVMIntPredicate pred[] = {
        LLVMIntEQ, LLVMIntNE, LLVMIntSLT, LLVMIntSLE,
        LLVMIntSGT, LLVMIntSGE, LLVMIntEQ, LLVMIntNE,
        LLVMIntULT, LLVMIntULE, LLVMIntUGT, LLVMIntUGE
    };

    if (op >= OPER_CMP_EQLA && op <= OPER_CMP_GEQA) {
        return pred[op-(addrsigned ? 12 : 6)-OPER_CMP_EQL];
    }
    if (op >= OPER_CMP_EQL && op <= OPER_CMP_GEQU) {
        return pred[op-OPER_CMP_EQL];
    }
    return 0;

} /* llvmgen_predfromop */

/*
 * llvmgen_assignment
 *
 * Generates a store operation from an assignment expression.
 */
LLVMValueRef
llvmgen_assignment (gencodectx_t gctx, expr_node_t *lhs, expr_node_t *rhs)
{
    LLVMBuilderRef builder = (gctx->curfn == 0 ? 0 : gctx->curfn->builder);
    LLVMValueRef rhsvalue, v, lhsaddr;
    LLVMTypeRef  lhstype, rhstype;
    llvm_accinfo_t accinfo;
    int shifts_required = 0;

    rhsvalue = llvmgen_expression(gctx, rhs, 0);
    if (rhsvalue == 0) {
        unsigned int bpval = machine_scalar_bits(gctx->mach);
        expr_signal(gctx->ectx, STC__EXPRVALRQ);
        rhsvalue = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx, bpval));
    }
    rhstype = LLVMTypeOf(rhsvalue);

    lhsaddr = llvmgen_addr_expression(gctx, lhs, &accinfo);
    if (lhsaddr == 0) {
        expr_signal(gctx->ectx, STC__ADDRVALRQ);
        return rhsvalue;
    }
    // If we're assigning into a field-reference with a non-zero
    // bit position or a non-CTCE size, we have to do some bit-shifting
    // to do the store.
    if (accinfo.posval != 0 || accinfo.sizeval != 0) {
        shifts_required = 1;
        lhstype = gctx->fullwordtype;
        if ((accinfo.flags & LLVMGEN_M_ACC_CONSTSIZ) != 0) {
            accinfo.sizeval = LLVMConstInt(gctx->fullwordtype, accinfo.size, 0);
        }
    } else if ((accinfo.flags & LLVMGEN_M_ACC_CONSTSIZ) != 0) {
        lhstype = LLVMIntTypeInContext(gctx->llvmctx, accinfo.size);
    } else {
        lhstype = gctx->fullwordtype;
    }
    lhsaddr = llvmgen_adjustval(gctx, lhsaddr, LLVMPointerType(lhstype, 0), 0);
    if (shifts_required) {
        LLVMValueRef neg1, srcmask, dstmask, rhstemp;

        if (LLVMGetTypeKind(rhstype) != LLVMIntegerTypeKind) {
            rhsvalue = llvmgen_adjustval(gctx, rhsvalue, gctx->fullwordtype, 0);
            rhstype = LLVMTypeOf(rhsvalue);
        } else {
            accinfo.sizeval = llvmgen_adjustval(gctx, accinfo.sizeval, rhstype, 0);
            accinfo.posval = llvmgen_adjustval(gctx, accinfo.posval, rhstype, 0);
        }

        neg1 = LLVMConstAllOnes(rhstype);
        v = LLVMBuildShl(builder, neg1, accinfo.sizeval, llvmgen_temp(gctx));
        srcmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildAnd(builder, rhsvalue, srcmask, llvmgen_temp(gctx));
        v = LLVMBuildShl(builder, v, accinfo.posval, llvmgen_temp(gctx));
        rhstemp = llvmgen_adjustval(gctx, v, lhstype, 0);

        v = LLVMBuildShl(builder, srcmask, accinfo.posval, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, lhstype, 0);
        dstmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildLoad(builder, lhsaddr, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, lhstype, (accinfo.flags & LLVMGEN_M_SEG_SIGNEXT) != 0);
        v = LLVMBuildAnd(builder, v, dstmask, llvmgen_temp(gctx));
        v = LLVMBuildOr(builder, v, rhstemp, llvmgen_temp(gctx));
    } else {
        v = llvmgen_adjustval(gctx, rhsvalue, lhstype, (accinfo.flags & LLVMGEN_M_SEG_SIGNEXT) != 0);
    }

    LLVMBuildStore(builder, v, lhsaddr);
    if ((accinfo.flags & LLVMGEN_M_SEG_VOLATILE) != 0) LLVMSetVolatile(v, 1);

    return rhsvalue;

} /* llvmgen_assignment */

/*
 * gen_fetch
 *
 * Generates a load operation for a fetch expression.
 */
static LLVMValueRef
gen_fetch (gencodectx_t gctx, expr_node_t *rhs, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    llvm_accinfo_t accinfo;
    LLVMValueRef addr, val;
    LLVMTypeRef type;
    int shifts_required = 0;
    int signext;

    // For field references with non-zero bit position, or with
    // non-CTCE size, we'll have to do bit shifting to extract
    // the field.
    addr = llvmgen_addr_expression(gctx, rhs, &accinfo);
    if (accinfo.posval != 0 || accinfo.sizeval != 0) {
        type = gctx->fullwordtype;
        if ((accinfo.flags & LLVMGEN_M_ACC_CONSTSIZ)) {
            accinfo.sizeval = LLVMConstInt(gctx->fullwordtype, accinfo.size, 0);
        }
        shifts_required = 1;
    } else if ((accinfo.flags & LLVMGEN_M_ACC_CONSTSIZ)) {
        type = LLVMIntTypeInContext(gctx->llvmctx, accinfo.size);
    } else {
        type = gctx->fullwordtype;
    }
    signext = ((accinfo.flags & LLVMGEN_M_SEG_SIGNEXT) != 0);

    // If we're fetching from a register, there's no load intruction
    // required.
    if (accinfo.segclass == LLVM_REG &&
        (accinfo.flags & (LLVMGEN_M_SEG_DEREFED|LLVMGEN_M_SEG_ISREF)) == 0) {
        val = llvmgen_adjustval(gctx, addr, type, signext);
    } else {
        addr = llvmgen_adjustval(gctx, addr, LLVMPointerType(type, 0), 0);
        val = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
        if ((accinfo.flags & LLVMGEN_M_SEG_VOLATILE) != 0) LLVMSetVolatile(val, 1);
    }
    if (shifts_required) {
        val = llvmgen_adjustval(gctx, val, gctx->fullwordtype, signext);
        if (signext) {
            val = LLVMBuildAShr(builder, val, accinfo.posval, llvmgen_temp(gctx));
        } else {
            val = LLVMBuildLShr(builder, val, accinfo.posval, llvmgen_temp(gctx));
        }

        if ((accinfo.flags & LLVMGEN_M_ACC_CONSTSIZ) != 0) {
            LLVMTypeRef trunctype = LLVMIntTypeInContext(gctx->llvmctx, accinfo.size);
            val = llvmgen_adjustval(gctx, val, trunctype, signext);
        } else {
            LLVMValueRef neg1 = LLVMConstAllOnes(gctx->fullwordtype);
            LLVMValueRef mask;

            mask = LLVMBuildShl(builder, neg1, accinfo.sizeval, llvmgen_temp(gctx));
            mask = LLVMBuildNeg(builder, mask, llvmgen_temp(gctx));
            val = LLVMBuildAnd(builder, val, mask, llvmgen_temp(gctx));
            if (signext) {
                val = LLVMBuildSExt(builder, val, gctx->fullwordtype, llvmgen_temp(gctx));
            }
        }
    }

    return llvmgen_adjustval(gctx, val, neededtype, signext);

} /* gen_fetch */

/*
 * gen_shift
 *
 * Shifts are a little tricky, since LLVM has explicit left-shift and
 * right-shift instructions, which take non-negative shift values.  BLISS,
 * on the other hand, has a single shift operator and generates right-shifts
 * when the RHS is negative.  If the RHS is a constant, we can do the translation
 * here; otherwise, we have to build a conditional to check at runtime.
 */
static LLVMValueRef
gen_shift (gencodectx_t gctx, expr_node_t *lhs, expr_node_t *rhs, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMTypeRef inttype = gctx->fullwordtype;
    LLVMValueRef lval, rval, result, test;

    lval = (lhs == 0 ? 0 : llvmgen_expression(gctx, lhs, inttype));

    if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
        long count = expr_litval(rhs);
        if (count < 0) {
            rval = LLVMConstInt(inttype, -count, 0);
            result = LLVMBuildLShr(builder, lval, rval, llvmgen_temp(gctx));
        } else {
            rval = LLVMConstInt(inttype, count, 0);
            result = LLVMBuildShl(builder, lval, rval, llvmgen_temp(gctx));
        }
    } else {
        LLVMBasicBlockRef exitblock = llvmgen_exitblock_create(gctx, 0);
        LLVMBasicBlockRef lshiftblk, rshiftblk;
        llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblock);

        lshiftblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblock, llvmgen_label(gctx));
        rshiftblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblock, llvmgen_label(gctx));

        rval = llvmgen_expression(gctx, rhs, inttype);
        test = LLVMBuildICmp(builder, LLVMIntSLT, rval, LLVMConstNull(inttype), llvmgen_temp(gctx));
        LLVMBuildCondBr(builder, test, rshiftblk, lshiftblk);
        LLVMPositionBuilderAtEnd(builder, lshiftblk);
        result = LLVMBuildShl(builder, lval, rval, llvmgen_temp(gctx));
        llvmgen_btrack_update(gctx, bt, result);
        LLVMPositionBuilderAtEnd(builder, rshiftblk);
        rval = LLVMBuildNeg(builder, rval, llvmgen_temp(gctx));
        result = LLVMBuildLShr(builder, lval, rval, llvmgen_temp(gctx));
        llvmgen_btrack_update(gctx, bt, result);
        result = llvmgen_btrack_finalize(gctx, bt, inttype);
    }

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_shift */

/*
 * gen_operator_expression
 *
 * Code generation for operator expressions.  Most of them have straightforward
 * translations into LLVM instructions and are handled directly here.
 */
static LLVMValueRef
gen_operator_expression (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    expr_node_t *lhs = expr_op_lhs(exp);
    expr_node_t *rhs = expr_op_rhs(exp);
    optype_t op = expr_op_type(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMTypeRef inttype;
    LLVMValueRef lval, rval, result;

    if (op == OPER_FETCH) {
        return gen_fetch(gctx, rhs, neededtype);
    }

    if (op == OPER_ASSIGN) {
        LLVMValueRef val = llvmgen_assignment(gctx, lhs, rhs);
        return llvmgen_adjustval(gctx, val, neededtype, 0);
    }

    if (op == OPER_SHIFT) {
        return gen_shift(gctx, lhs, rhs, neededtype);
    }

    inttype = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(gctx->mach));

    lval = (lhs == 0 ? 0 : llvmgen_expression(gctx, lhs, inttype));
    rval = llvmgen_expression(gctx, rhs, inttype);
    switch (op) {
        case OPER_UNARY_PLUS:
            result = rval;
            break;
        case OPER_UNARY_MINUS:
            result = LLVMBuildNeg(builder, rval, llvmgen_temp(gctx));
            break;
        case OPER_ADD:
            result = LLVMBuildAdd(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_SUBTRACT:
            result = LLVMBuildSub(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_MULT:
            result = LLVMBuildMul(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_DIV:
            result = LLVMBuildUDiv(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_MODULO:
            result = LLVMBuildURem(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_AND:
            result = LLVMBuildAnd(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_OR:
            result = LLVMBuildOr(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_NOT:
            result = LLVMBuildNot(builder, rval, llvmgen_temp(gctx));
            break;
        case OPER_XOR:
            result = LLVMBuildXor(builder, lval, rval, llvmgen_temp(gctx));
            break;
        case OPER_EQV:
            result = LLVMBuildXor(builder, lval, rval, llvmgen_temp(gctx));
            result = LLVMBuildNot(builder, result, llvmgen_temp(gctx));
            break;
        default:
            if (op >= OPER_CMP_EQL && op <= OPER_CMP_GEQA) {
                result = LLVMBuildICmp(builder,
                                       llvmgen_predfromop(op, machine_addr_signed(gctx->mach)),
                                       lval, rval, llvmgen_temp(gctx));
            } else {
                // Everything should be covered
                expr_signal(gctx->ectx, STC__INTCMPERR, "gen_operator_expression");
                result = LLVMConstNull(inttype);
            }
            break;
    }

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_operator_expression */

/*
 * llvmgen_opexpgen_init
 *
 * Registers the expression-type handler with the dispatcher.
 */
void
llvmgen_opexpgen_init (gencodectx_t gctx)
{
    llvmgen_expgen_register(gctx, EXPTYPE_OPERATOR, gen_operator_expression);

} /* llvmgen_opexpgen_init */
