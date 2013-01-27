/*
 *++
 *	File:			gencode_llvm.c
 *
 *	Abstract:		Common codegen to LLVM IR.
 *
 *  Module description:
 *
 *      This module contains the main code generation entry
 *      points and some common routines shared by the other
 *      llvmgen modules.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, 2013  Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		28-Dec-2012	V1.0	Madison		Initial coding.
 *      24-Jan-2013 V1.1    Madison     After refactoring.
 *--
 */

#include "llvmgen.h"
#include "blissc/switches.h"
#include <assert.h>

LLVMBasicBlockRef
llvmgen_exitblock_create (gencodectx_t gctx, char *label)
{
    LLVMBasicBlockRef insertpoint;
    char *l = (label == 0 ? llvmgen_label(gctx) : label);
    insertpoint = LLVMGetNextBasicBlock(LLVMGetInsertBlock(gctx->curfn->builder));
    return (insertpoint == 0
            ? LLVMAppendBasicBlockInContext(gctx->llvmctx, gctx->curfn->func, l)
            : LLVMInsertBasicBlockInContext(gctx->llvmctx, insertpoint, l));

} /* llvmgen_create_exitblock */

llvm_btrack_t *
llvmgen_btrack_create (gencodectx_t gctx, LLVMBasicBlockRef exitpoint) {
    llvm_btrack_t *bt;
    if (gctx->freebts == 0) {
        bt = malloc(sizeof(llvm_btrack_t));
    } else {
        bt = gctx->freebts;
        gctx->freebts = bt->next;
    }
    memset(bt, 0, sizeof(llvm_btrack_t));
    bt->exitblock = exitpoint;
    return bt;

} /* llvmgen_btrack_create */

LLVMValueRef
llvmgen_btrack_finalize (gencodectx_t gctx, llvm_btrack_t *bt, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef phi = 0;

    if (bt == 0) return 0;
    if (bt->branchcount == 0) {
        LLVMDeleteBasicBlock(bt->exitblock);
    } else {
        LLVMPositionBuilderAtEnd(builder, bt->exitblock);
        if (bt->phirefcount > 0 && bt->phirefcount == bt->branchcount) {
            phi = LLVMBuildPhi(builder, gctx->fullwordtype, llvmgen_temp(gctx));
            LLVMAddIncoming(phi, bt->phivals, bt->phisources, bt->phirefcount);
        }
    }
    bt->next = gctx->freebts;
    gctx->freebts = bt;
    return (phi == 0 ? 0 : llvmgen_adjustval(gctx, phi, neededtype, 1));

} /* llvmgen_btrack_finalize */

/*
 * gencode_routine_begin
 */
int
gencode_routine_begin (gencodectx_t gctx, name_t *np)
{
    LLVMValueRef thisfn = LLVMGetNamedFunction(gctx->module, name_azstring(np));
    llvm_rtntrack_t *rt;
    LLVMBasicBlockRef entryblk, exitblk;

    if (gctx->freerts == 0) {
        rt = malloc(sizeof(llvm_rtntrack_t));
    } else {
        rt = gctx->freerts;
        gctx->freerts = rt->next;
    }
    memset(rt, 0, sizeof(llvm_rtntrack_t));
    rt->next = gctx->curfn;
    gctx->curfn = rt;
    entryblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                             llvmgen_label(gctx));
    rt->builder = LLVMCreateBuilderInContext(gctx->llvmctx);
    LLVMPositionBuilderAtEnd(rt->builder, entryblk);
    exitblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn,
                                            llvmgen_label(gctx));
    rt->btrack[LLVMGEN_K_BT_FUNC] = llvmgen_btrack_create(gctx, exitblk);
    rt->func = thisfn;

    return 1;

} /* gencode_routine_begin */

/*
 * gencode_routine_end
 */
int
gencode_routine_end (gencodectx_t gctx, name_t *np)
{
    routine_attr_t *attr = rtnsym_attr(np);
    expr_node_t *rtnexpr = rtnsym_expr(np);
    int novalue = ((attr->flags & SYM_M_NOVALUE) != 0);
    llvm_rtntrack_t *rt = gctx->curfn;
    LLVMValueRef thisfn = rt->func;
    LLVMValueRef val;

    LLVMPositionBuilderAtEnd(rt->builder, LLVMGetEntryBasicBlock(thisfn));
    val = llvmgen_expression(gctx, rtnexpr, (novalue ? 0 : gctx->fullwordtype));
    llvmgen_btrack_update(gctx, rt->btrack[LLVMGEN_K_BT_FUNC], (novalue ? 0 : val));
    val = llvmgen_btrack_finalize(gctx, rt->btrack[LLVMGEN_K_BT_FUNC],
                                  (novalue ? 0 : gctx->fullwordtype));
    if (novalue) {
        LLVMBuildRetVoid(rt->builder);
    } else {
        if (val == 0) {
            expr_signal(gctx->ectx, STC__EXPRVALRQ);
            val = LLVMConstNull(gctx->fullwordtype);
        }
        LLVMBuildRet(rt->builder, val);
    }
    LLVMDisposeBuilder(rt->builder);
    gctx->curfn = rt->next;
    rt->next = gctx->freerts;
    gctx->freerts = rt;

    LLVMVerifyFunction(thisfn, LLVMPrintMessageAction);
    LLVMRunFunctionPassManager(gctx->passmgr, thisfn);

    return 1;

} /* gencode_routine_end */

void
llvmgen_memcpy (gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef src, LLVMValueRef len)
{
    LLVMValueRef args[5];

    if (gctx->memcpyfn == 0) {
        LLVMTypeRef type;

        gctx->memcpyargtypes[0] = LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0);
        gctx->memcpyargtypes[1] = gctx->memcpyargtypes[0];
        gctx->memcpyargtypes[2] = LLVMInt32TypeInContext(gctx->llvmctx);
        gctx->memcpyargtypes[3] = gctx->memcpyargtypes[2];
        gctx->memcpyargtypes[4] = gctx->int1type;
        type = LLVMFunctionType(LLVMVoidTypeInContext(gctx->llvmctx), gctx->memcpyargtypes, 5, 0);
        gctx->memcpyfn = LLVMAddFunction(gctx->module, "llvm.memcpy.p0i8.p0i8.i32", type);
    }

    if (gctx->curfn == 0 || gctx->curfn->builder == 0 || gctx->memcpyfn == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_memcpy");
        return;
    }

    args[0] = llvmgen_adjustval(gctx, dest, gctx->memcpyargtypes[0], machine_addr_signed(gctx->mach));
    args[1] = llvmgen_adjustval(gctx, src, gctx->memcpyargtypes[1], machine_addr_signed(gctx->mach));
    args[2] = llvmgen_adjustval(gctx, len, gctx->memcpyargtypes[2], 0);
    args[3] = LLVMConstNull(gctx->memcpyargtypes[3]);
    args[4] = LLVMConstNull(gctx->memcpyargtypes[4]);
    LLVMBuildCall(gctx->curfn->builder, gctx->memcpyfn, args, 5, "");
    
} /* llvmgen_memcpy */


void
llvmgen_memset (gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef uval, LLVMValueRef len)
{
    LLVMValueRef args[5];

    if (gctx->memsetfn == 0) {
        LLVMTypeRef type;

        gctx->memsetargtypes[1] = LLVMInt8TypeInContext(gctx->llvmctx);
        gctx->memsetargtypes[0] = LLVMPointerType(gctx->memsetargtypes[1], 0);
        gctx->memsetargtypes[2] = LLVMInt32TypeInContext(gctx->llvmctx);
        gctx->memsetargtypes[3] = gctx->memcpyargtypes[2];
        gctx->memsetargtypes[4] = gctx->int1type;
        type = LLVMFunctionType(LLVMVoidTypeInContext(gctx->llvmctx), gctx->memsetargtypes, 5, 0);
        gctx->memsetfn = LLVMAddFunction(gctx->module, "llvm.memset.p0i8.i32", type);
    }

    if (gctx->curfn == 0 || gctx->curfn->builder == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_memset");
        return;
    }

    args[0] = llvmgen_adjustval(gctx, dest, gctx->memsetargtypes[0], machine_addr_signed(gctx->mach));
    args[1] = llvmgen_adjustval(gctx, uval, gctx->memsetargtypes[1], 0);
    args[2] = llvmgen_adjustval(gctx, len, gctx->memsetargtypes[2], 0);
    args[3] = LLVMConstNull(gctx->memsetargtypes[3]);
    args[4] = LLVMConstNull(gctx->memsetargtypes[4]);
    LLVMBuildCall(gctx->curfn->builder, gctx->memsetfn, args, 5, "");

} /* llvmgen_memset */

LLVMValueRef
llvmgen_cast_trunc_ext (gencodectx_t gctx, LLVMValueRef val, LLVMTypeRef neededtype, int signext)
{
    LLVMTypeRef valtype = LLVMTypeOf(val);
    LLVMTypeKind valkind, needkind;
    LLVMBuilderRef builder;
    LLVMValueRef tmp;
    int constcast;

    if (neededtype == 0 || valtype == neededtype) return val;
    valkind = LLVMGetTypeKind(valtype);
    assert(valkind == LLVMPointerTypeKind || valkind == LLVMIntegerTypeKind);
    needkind = LLVMGetTypeKind(neededtype);
    assert(needkind == LLVMPointerTypeKind || needkind == LLVMIntegerTypeKind);
    constcast = LLVMIsConstant(val);
    builder = (gctx->curfn == 0 ? 0 : gctx->curfn->builder);
    if (!constcast && (gctx->curfn == 0 || builder == 0)) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_cast_trunc_ext");
        return 0;
    }
    if (valkind == LLVMPointerTypeKind) {
        if (needkind == LLVMPointerTypeKind) {
            if (constcast) {
                return LLVMConstPointerCast(val, neededtype);
            }
            return LLVMBuildPointerCast(builder, val, neededtype, llvmgen_temp(gctx));
        }
        if (constcast) {
            return llvmgen_adjustval(gctx, LLVMConstPtrToInt(val, gctx->intptrtszype),
                                     neededtype, signext);
        }
        tmp = LLVMBuildPtrToInt(builder, val, gctx->intptrtszype, llvmgen_temp(gctx));
        return llvmgen_adjustval(gctx, tmp, neededtype, signext);
    }
    // Value is an integer
    if (needkind == LLVMIntegerTypeKind) {
        unsigned int valbits = LLVMGetIntTypeWidth(valtype);
        unsigned int needbits = LLVMGetIntTypeWidth(neededtype);
        if (needbits < valbits) {
            if (constcast) {
                return LLVMConstTrunc(val, neededtype);
            }
            return LLVMBuildTrunc(builder, val, neededtype, llvmgen_temp(gctx));
        } else if (needbits > valbits) {
            if (constcast) {
                return (signext ? LLVMConstSExt(val, neededtype)
                        : LLVMConstZExt(val, neededtype));
            }
            if (signext) {
                return LLVMBuildSExt(builder, val, neededtype, llvmgen_temp(gctx));
            }
            return LLVMBuildZExt(builder, val, neededtype, llvmgen_temp(gctx));
        }
    } else if (needkind == LLVMPointerTypeKind) {
        val = llvmgen_adjustval(gctx, val, gctx->intptrtszype, machine_addr_signed(gctx->mach));
        return (constcast ? LLVMConstIntToPtr(val, neededtype)
                : LLVMBuildIntToPtr(builder, val, neededtype, llvmgen_temp(gctx)));
    }

    // XXX I don't think this would ever happen
    return (constcast ? LLVMConstBitCast(val, neededtype)
            : LLVMBuildBitCast(builder, val, neededtype, llvmgen_temp(gctx)));

} /* llvmgen_cast_trunc_ext */

static int
optlevel_handler (parse_ctx_t pctx, void *vctx, lextype_t dcltype, lexeme_t *swlex)
{
    gencodectx_t gctx = vctx;
    lexeme_t *lex;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
        expr_signal(gctx->ectx, STC__DELIMEXP, "=");
        return 1;
    }

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NUMERIC, &lex, 1)) {
        expr_signal(gctx->ectx, STC__INVOPTLVL);
        return 1;
    }
    if (!string_numval(lexeme_text(lex), 10, &val) || val < 0 || val > 3) {
        expr_signal(gctx->ectx, STC__INVOPTLVL);
    }

    gctx->optlevel = (unsigned int) val;

    return 1;

} /* optlevel_handler */

/*
 * gencode_module_begin
 */
int
gencode_module_begin (gencodectx_t gctx, name_t *modnp)
{
    char *dl;

    llvmgen_expgen_init(gctx);

    gctx->modnp = modnp;
    gctx->module = LLVMModuleCreateWithNameInContext(name_azstring(modnp), gctx->llvmctx);
    gctx->passmgr = LLVMCreateFunctionPassManagerForModule(gctx->module);

    LLVMSetTarget(gctx->module, LLVMGetTargetMachineTriple(gctx->mctx->target_machine));
    dl = LLVMCopyStringRepOfTargetData(LLVMGetTargetMachineData(gctx->mctx->target_machine));
    LLVMSetDataLayout(gctx->module, dl);
    LLVMDisposeMessage(dl);

    LLVMAddBasicAliasAnalysisPass(gctx->passmgr);
    if (gctx->optlevel > 0) {
        LLVMAddInstructionCombiningPass(gctx->passmgr);
        LLVMAddReassociatePass(gctx->passmgr);
        LLVMAddGVNPass(gctx->passmgr);
        LLVMAddCFGSimplificationPass(gctx->passmgr);
    }

    return (gctx->module != 0);

} /* gencode_module_begin */

int
gencode_module_end (gencodectx_t gctx, name_t *np)
{
    char *err;

    LLVMVerifyModule(gctx->module, LLVMPrintMessageAction, 0);
//    LLVMDumpModule(gctx->module);
    err = 0;
    if (LLVMTargetMachineEmitToFile(gctx->mctx->target_machine, gctx->module,
                                    gctx->mctx->outfile, gctx->mctx->outputtype, &err)) {
        if (err) { fprintf(stderr, "%s\n", err); LLVMDisposeMessage(err); }
    }
    LLVMDisposePassManager(gctx->passmgr);
    LLVMDisposeModule(gctx->module);

    return 1;

} /* gencode_module_end */

void
gencode_optlevel_set (gencodectx_t gctx, unsigned int level)
{
    gctx->optlevel = (level & 3);  // valid values are 0, 1, 2, 3
}

void
gencode_postinit (gencodectx_t gctx)
{
    switch_special_declare(parser_kwdscope(expr_parse_ctx(gctx->ectx)), LEXTYPE_SWITCH_OPTLEVEL,
                           optlevel_handler, gctx);

} /* gencode_postinit */

/*
 * gencode_init
 *
 * Module initialization.
 */
gencodectx_t
gencode_init (void *ectx, logctx_t logctx, machinedef_t *mach, symctx_t symctx)
{
    gencodectx_t gctx = malloc(sizeof(struct gencodectx_s));
    machine_ctx_t mctx = machine_context(mach);

    if (gctx == 0) return 0;
    memset(gctx, 0, sizeof(struct gencodectx_s));
    gctx->ectx = ectx;
    gctx->mach = mach;
    mctx->genctx = gctx;
    gctx->symctx = symctx;
    gctx->mctx = machine_context(mach);
    gctx->llvmctx = gctx->mctx->llvmctx;
    gctx->int1type = LLVMIntTypeInContext(gctx->llvmctx, 1);
    gctx->fullwordtype = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(mach));
    gctx->unitptrtype = LLVMPointerType(LLVMIntTypeInContext(gctx->llvmctx,
                                                             machine_unit_bits(mach)), 0);
    gctx->intptrtszype = LLVMIntTypeInContext(gctx->llvmctx, machine_addr_bits(mach));
    gctx->optlevel = 1;

    llvmgen_symgen_init(gctx);

    return gctx;

} /* gencode_init */
