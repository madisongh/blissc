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
#include "nametable.h"

static LLVMIntPredicate pred[] = {
    LLVMIntEQ, LLVMIntNE, LLVMIntSLT, LLVMIntSLE,
    LLVMIntSGT, LLVMIntSGE, LLVMIntEQ, LLVMIntNE,
    LLVMIntULT, LLVMIntULE, LLVMIntUGT, LLVMIntUGE
};

LLVMIntPredicate
llvmgen_predfromop (optype_t op, int addrsigned)
{
    if (op >= OPER_CMP_EQLA && op <= OPER_CMP_GEQA) {
        return pred[op-(addrsigned ? 12 : 6)];
    }
    if (op >= OPER_CMP_EQL && op <= OPER_CMP_GEQU) {
        return pred[op];
    }
    return 0;
}

LLVMValueRef
llvmgen_assignment (gencodectx_t gctx, expr_node_t *lhs, expr_node_t *rhs)
{

    LLVMValueRef rhsvalue, v, lhsaddr;
    LLVMTypeRef  lhstype;
    LLVMBuilderRef builder = (gctx->curfn == 0 ? 0 : gctx->curfn->builder);

    rhsvalue = llvmgen_expression(gctx, rhs, 0);
    if (rhsvalue == 0) {
        unsigned int bpval = machine_scalar_bits(gctx->mach);
        expr_signal(gctx->ectx, STC__EXPRVALRQ);
        rhsvalue = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx, bpval));
    }
    lhsaddr = llvmgen_addr_expression(gctx, lhs);
    if (lhsaddr == 0) {
        expr_signal(gctx->ectx, STC__ADDRVALRQ);
        return rhsvalue;
    }
    lhstype = LLVMGetElementType(LLVMTypeOf(lhsaddr));
    if (expr_type(lhs) == EXPTYPE_PRIM_FLDREF) {
        LLVMTargetDataRef target = LLVMGetTargetMachineData(gctx->mctx->target_machine);
        unsigned int neededwidth;
        LLVMValueRef pos, size, neg1, srcmask, dstmask, rhstemp;
        LLVMTypeRef inttype, rhstype;

        rhstype = LLVMTypeOf(rhsvalue);
        neededwidth = (unsigned int)LLVMSizeOfTypeInBits(target, lhstype);
        pos = llvmgen_expression(gctx, expr_fldref_pos(lhs), rhstype);
        size = llvmgen_expression(gctx, expr_fldref_size(lhs), rhstype);
        inttype = LLVMIntTypeInContext(gctx->llvmctx, neededwidth);
        neg1 = LLVMConstAllOnes(rhstype);

        v = LLVMBuildShl(builder, neg1, size, llvmgen_temp(gctx));
        srcmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildAnd(builder, rhsvalue, srcmask, llvmgen_temp(gctx));
        v = LLVMBuildShl(builder, v, pos, llvmgen_temp(gctx));
        rhstemp = llvmgen_adjustval(gctx, v, inttype);

        v = LLVMBuildShl(builder, srcmask, pos, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, inttype);
        dstmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildLoad(builder, lhsaddr, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, inttype);
        v = LLVMBuildAnd(builder, v, dstmask, llvmgen_temp(gctx));
        v = LLVMBuildOr(builder, v, rhstemp, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, lhstype);
    } else {
        v = llvmgen_adjustval(gctx, rhsvalue, lhstype);
    }

    LLVMBuildStore(builder, v, lhsaddr);

    return rhsvalue;

} /* llvmgen_assignment */

static LLVMValueRef
gen_fetch (gencodectx_t gctx, expr_node_t *rhs, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef addr, val;
    int signext = 0;
    expr_node_t *base = (expr_type(rhs) == EXPTYPE_PRIM_FLDREF ? expr_fldref_addr(rhs) : rhs);

    if (expr_type(base) == EXPTYPE_PRIM_SEG && name_type(expr_seg_name(base)) == LEXTYPE_NAME_DATA) {
        llvm_stgclass_t valclass;
        addr = llvmgen_segaddress(gctx, expr_seg_name(base), &valclass, &signext);
        if (valclass == LLVM_REG) {
            val = addr;
        } else {
            val = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
        }
    } else {
        addr = llvmgen_addr_expression(gctx, rhs);
        val = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
    }
    // If this is a field reference, do the extraction
    if (rhs != base) {
        LLVMTypeRef inttype = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(gctx->mach));
        expr_node_t *pos = expr_fldref_pos(rhs);
        expr_node_t *size = expr_fldref_size(rhs);

        val = llvmgen_adjustval(gctx, val, inttype);
        if (!(expr_type(pos) == EXPTYPE_PRIM_LIT && expr_litval(pos) == 0)) {
            LLVMValueRef pval = llvmgen_expression(gctx, pos, inttype);
            if (signext) {
                val = LLVMBuildAShr(builder, val, pval, llvmgen_temp(gctx));
            } else {
                val = LLVMBuildLShr(builder, val, pval, llvmgen_temp(gctx));
            }
        }

        signext = expr_fldref_signext(rhs);

        if (expr_type(size) == EXPTYPE_PRIM_LIT) {
            LLVMTypeRef trunctype = LLVMIntTypeInContext(gctx->llvmctx,
                                                         (unsigned int) expr_litval(size));
            val = llvmgen_adjustval(gctx, val, trunctype);
        } else {
            LLVMValueRef neg1 = LLVMConstAllOnes(inttype);
            LLVMValueRef sizeval = llvmgen_expression(gctx, size, inttype);
            LLVMValueRef mask;

            mask = LLVMBuildShl(builder, neg1, sizeval, llvmgen_temp(gctx));
            mask = LLVMBuildNeg(builder, mask, llvmgen_temp(gctx));
            val = LLVMBuildAnd(builder, val, mask, llvmgen_temp(gctx));
            if (signext) val = LLVMBuildSExt(builder, val, inttype, llvmgen_temp(gctx));
        }
    }

    if (neededtype != 0) {
        val = llvmgen_adjustval(gctx, val, neededtype);
    }

    return val;

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
    LLVMTypeRef inttype;
    LLVMValueRef lval, rval, result, test;

    inttype = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(gctx->mach));

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
        result = llvmgen_btrack_finalize(gctx, bt);
    }
    if (neededtype != 0) {
        result = llvmgen_adjustval(gctx, result, neededtype);
    }

    return result;

} /* gen_shift */

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
        if (neededtype != 0) {
            val = llvmgen_adjustval(gctx, val, neededtype);
        }
        return val;
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
    if (neededtype != 0) {
        result = llvmgen_adjustval(gctx, result, neededtype);
    }

    return result;

} /* gen_operator_expression */

/*
 * llvmgen_opexpgen_init
 */
void
llvmgen_opexpgen_init (gencodectx_t gctx)
{
    llvmgen_expgen_register(gctx, EXPTYPE_OPERATOR, gen_operator_expression);

} /* llvmgen_opexpgen_init */
