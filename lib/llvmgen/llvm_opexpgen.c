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
        return pred[op-(addrsigned ? 12 : 6)-OPER_CMP_EQL];
    }
    if (op >= OPER_CMP_EQL && op <= OPER_CMP_GEQU) {
        return pred[op-OPER_CMP_EQL];
    }
    return 0;
}

LLVMValueRef
llvmgen_assignment (gencodectx_t gctx, expr_node_t *lhs, expr_node_t *rhs)
{
    expr_node_t *lhsbase = (expr_type(lhs) == EXPTYPE_PRIM_STRUREF ? expr_struref_accexpr(lhs) : lhs);
    LLVMValueRef rhsvalue, v, lhsaddr, size, pos;
    LLVMTypeRef  lhstype, rhstype;
    LLVMBuilderRef builder = (gctx->curfn == 0 ? 0 : gctx->curfn->builder);
    unsigned int flags = 0;
    int signext = ((flags & LLVMGEN_M_SEG_SIGNEXT) != 0);
    int shifts_required = 0;

    rhsvalue = llvmgen_expression(gctx, rhs, 0);
    if (rhsvalue == 0) {
        unsigned int bpval = machine_scalar_bits(gctx->mach);
        expr_signal(gctx->ectx, STC__EXPRVALRQ);
        rhsvalue = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx, bpval));
    }
    lhsaddr = llvmgen_addr_expression(gctx, lhs, &flags);
    if (lhsaddr == 0) {
        expr_signal(gctx->ectx, STC__ADDRVALRQ);
        return rhsvalue;
    }
    lhstype = LLVMGetElementType(LLVMTypeOf(lhsaddr));
    if (LLVMGetTypeKind(lhstype) == LLVMArrayTypeKind || LLVMGetTypeKind(lhstype) == LLVMStructTypeKind
        || expr_type(lhsbase) == EXPTYPE_PRIM_FLDREF) {
        lhstype = gctx->fullwordtype;
        lhsaddr = llvmgen_adjustval(gctx, lhsaddr, LLVMPointerType(lhstype, 0), 0);
    }

    rhstype = LLVMTypeOf(rhsvalue);
    if (expr_type(lhsbase) == EXPTYPE_PRIM_FLDREF) {
        expr_node_t *pexp = expr_fldref_pos(lhsbase);
        expr_node_t *sexp = expr_fldref_size(lhsbase);
        LLVMTypeRef type;

        signext = expr_fldref_signext(lhsbase);

        shifts_required = 1;
        type = (LLVMGetTypeKind(rhstype) == LLVMIntegerTypeKind ? rhstype : gctx->fullwordtype);
        size = llvmgen_expression(gctx, sexp, type);
        pos = llvmgen_expression(gctx, pexp, type);
    } else if (expr_type(lhsbase) == EXPTYPE_PRIM_SEG) {
        llvm_stgclass_t segclass;
        int was_deref;
        was_deref = (expr_type(lhs) == EXPTYPE_PRIM_STRUREF &&
                     expr_struref_referer(lhs) == expr_seg_name(lhsbase));
        llvmgen_segaddress(gctx, expr_seg_name(lhsbase), &segclass, 0);
        if (segclass == LLVM_REG && !was_deref &&
            expr_seg_offset(lhsbase) != 0 && expr_seg_width(lhsbase) != machine_scalar_bits(gctx->mach)) {
            LLVMTypeRef type;

            shifts_required = 1;
            type = (LLVMGetTypeKind(rhstype) == LLVMIntegerTypeKind ? rhstype : gctx->fullwordtype);
            size = LLVMConstInt(type, expr_seg_width(lhsbase), 0);
            pos = LLVMConstInt(type, expr_seg_offset(lhsbase) * machine_unit_bits(gctx->mach), 0);
        } else {
            LLVMTypeRef sizedtype = LLVMIntTypeInContext(gctx->llvmctx, expr_seg_width(lhsbase));
            if (lhstype != sizedtype) {
                lhsaddr = llvmgen_adjustval(gctx, lhsaddr, LLVMPointerType(sizedtype, 0), 0);
                lhstype = sizedtype;
            }
        }
    }

    if (shifts_required) {
        LLVMValueRef neg1, srcmask, dstmask, rhstemp;

        if (LLVMGetTypeKind(rhstype) != LLVMIntegerTypeKind) {
            rhsvalue = llvmgen_adjustval(gctx, rhsvalue, gctx->fullwordtype, 0);
            rhstype = LLVMTypeOf(rhsvalue);
        }

        neg1 = LLVMConstAllOnes(rhstype);
        v = LLVMBuildShl(builder, neg1, size, llvmgen_temp(gctx));
        srcmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildAnd(builder, rhsvalue, srcmask, llvmgen_temp(gctx));
        v = LLVMBuildShl(builder, v, pos, llvmgen_temp(gctx));
        rhstemp = llvmgen_adjustval(gctx, v, lhstype, 0);

        v = LLVMBuildShl(builder, srcmask, pos, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, lhstype, 0);
        dstmask = LLVMBuildNot(builder, v, llvmgen_temp(gctx));
        v = LLVMBuildLoad(builder, lhsaddr, llvmgen_temp(gctx));
        v = llvmgen_adjustval(gctx, v, lhstype, signext);
        v = LLVMBuildAnd(builder, v, dstmask, llvmgen_temp(gctx));
        v = LLVMBuildOr(builder, v, rhstemp, llvmgen_temp(gctx));
    } else {
        v = llvmgen_adjustval(gctx, rhsvalue, lhstype, signext);
    }

    LLVMBuildStore(builder, v, lhsaddr);
    if ((flags & LLVMGEN_M_SEG_VOLATILE) != 0) LLVMSetVolatile(v, 1);

    return rhsvalue;

} /* llvmgen_assignment */

static LLVMValueRef
gen_fetch (gencodectx_t gctx, expr_node_t *rhsactual, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    unsigned int flags = 0;
    expr_node_t *base, *rhs;
    unsigned int bpval = machine_scalar_bits(gctx->mach);
    LLVMValueRef addr, val;
    int signext;

    rhs = (expr_type(rhsactual) == EXPTYPE_PRIM_STRUREF ? expr_struref_accexpr(rhsactual) : rhsactual);
    base = (expr_type(rhs) == EXPTYPE_PRIM_FLDREF ? expr_fldref_addr(rhs) : rhs);
    if (expr_type(base) == EXPTYPE_PRIM_SEG && name_type(expr_seg_name(base)) == LEXTYPE_NAME_DATA) {
        data_attr_t *attr = datasym_attr(expr_seg_name(base));
        unsigned int width = expr_seg_width(base);
        unsigned int offset = (unsigned int) expr_seg_offset(base);
        llvm_stgclass_t valclass;

        if (width == 0) {
            width = attr->width;
        }
        if (rhs != rhsactual) { // it's a STRUREF, make sure auto-deref happens
            llvmgen_deref_push(gctx, expr_seg_name(base));
        }
        addr = llvmgen_segaddress(gctx, expr_seg_name(base), &valclass, &flags);
        if (rhs != rhsactual) {
            llvmgen_deref_pop(gctx, expr_seg_name(base));
        }
        if (valclass == LLVM_REG && (flags & LLVMGEN_M_SEG_DEREFED) == 0) {
            if (width != bpval) {
                addr = llvmgen_adjustval(gctx, addr, gctx->fullwordtype, 0);
                val = LLVMBuildTrunc(builder, addr,
                                     LLVMIntTypeInContext(gctx->llvmctx, width),
                                     llvmgen_temp(gctx));
            } else {
                val = addr;
            }
        } else {
            LLVMTypeRef fetchtype = LLVMPointerType(LLVMIntTypeInContext(gctx->llvmctx, width), 0);

            if (offset != 0) {
                LLVMValueRef off = LLVMConstInt(gctx->fullwordtype, offset, 1);
                addr = llvmgen_adjustval(gctx, addr, gctx->unitptrtype, 0);
                addr = LLVMBuildGEP(builder, addr, &off, 1, llvmgen_temp(gctx));
                addr = llvmgen_adjustval(gctx, addr, fetchtype, 0);
            } else if (LLVMTypeOf(addr) != fetchtype) {
                addr = LLVMBuildPointerCast(builder, addr, fetchtype, llvmgen_temp(gctx));
            }
            val = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
            if ((flags & LLVMGEN_M_SEG_VOLATILE) != 0) LLVMSetVolatile(val, 1);
        }
    } else {
        LLVMTypeRef fetchtype = LLVMPointerType(gctx->fullwordtype, 0);
        addr = llvmgen_addr_expression(gctx, rhs, &flags);
        if (LLVMTypeOf(addr) != fetchtype) {
            addr = LLVMBuildPointerCast(builder, addr, fetchtype, llvmgen_temp(gctx));
        }
        val = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
        if ((flags & LLVMGEN_M_SEG_VOLATILE) != 0) LLVMSetVolatile(val, 1);
    }
    signext = (flags & LLVMGEN_M_SEG_SIGNEXT) != 0;
    // If this is a field reference, do the extraction
    if (rhs != base) {
        LLVMTypeRef inttype = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(gctx->mach));
        expr_node_t *pos = expr_fldref_pos(rhs);
        expr_node_t *size = expr_fldref_size(rhs);

        val = llvmgen_adjustval(gctx, val, inttype, signext);
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
            val = llvmgen_adjustval(gctx, val, trunctype, signext);
        } else {
            LLVMValueRef neg1 = LLVMConstAllOnes(inttype);
            LLVMValueRef sizeval = llvmgen_expression(gctx, size, inttype);
            LLVMValueRef mask;

            mask = LLVMBuildShl(builder, neg1, sizeval, llvmgen_temp(gctx));
            mask = LLVMBuildNeg(builder, mask, llvmgen_temp(gctx));
            val = LLVMBuildAnd(builder, val, mask, llvmgen_temp(gctx));
            if (signext) {
                val = LLVMBuildSExt(builder, val, inttype, llvmgen_temp(gctx));
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
 */
void
llvmgen_opexpgen_init (gencodectx_t gctx)
{
    llvmgen_expgen_register(gctx, EXPTYPE_OPERATOR, gen_operator_expression);

} /* llvmgen_opexpgen_init */
