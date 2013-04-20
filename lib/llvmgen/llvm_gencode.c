/*
 *++
 * llvm_gencode.c - Common codegen to LLVM IR.
 *
 *
 * This module contains the main code generation entry
 * points and some common routines shared by the other
 * llvmgen modules.
 *
 * Copyright © 2012, 2013  Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "llvmgen.h"
#include "blissc/switches.h"
#include <assert.h>

/*
 * llvmgen_exitblock_create
 *
 * Utility routine for setting up an exit block for a control
 * sequence.
 */
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

/*
 * llvmgen_btrack_create
 *
 * Initializes a branch tracking structure.
 */
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

/*
 * llvmgen_btrack_finalize
 *
 * Finalizes a control sequence that uses branch tracking.  Inserts
 * the terminating phi in the exit block, or deleting the exit block
 * if there have been no branches to it.
 */
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
            phi = LLVMBuildPhi(builder, LLVMTypeOf(bt->phivals[0]), llvmgen_temp(gctx));
            LLVMAddIncoming(phi, bt->phivals, bt->phisources, bt->phirefcount);
        }
    }
    bt->next = gctx->freebts;
    gctx->freebts = bt;
    return (phi == 0 ? 0 : llvmgen_adjustval(gctx, phi, neededtype, 1));

} /* llvmgen_btrack_finalize */

/*
 * gencode_routine_begin
 *
 * Sets up the tracking structure for generating code in a routine
 * (function, in LLVM parlance).  Called by the front end just before
 * parsing a routine's expression.  Note that BLISS allows nested
 * routines, so these tracking structures are stacked.
 *
 * Two basic blocks are created here - the entry block and an exit
 * block.  The exit block will be deleted later if it is not needed.
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
 *
 * Performs the code generation for a routine.  Called by
 * the front end just after parsing a routine's expression.
 * The code for the expression is generated, the exit block
 * is finalized (to handle RETURNs), and the return value
 * (or void for NOVALUE routines) is generated.
 *
 * The generated LLVM is then verified for consistency, and
 * the function pass manager is called to process the code
 * and perform function-level optimizations.
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

/*
 * llvmgen_cast_trunc_ext
 *
 * Workhorse routine for generating a cast, truncation, or zero-/signed-extension
 * on an LLVM value to get it to a desired type.  This is called by the inlined
 * routine llvmgen_adjustval(), which checks for the degenerate cases where no
 * such transformation is required.  In most cases, that function should be
 * called, rather than calling this function directly.
 */
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
        if (constcast) {
            return LLVMConstIntToPtr(val, neededtype);
        } else {
            assert(gctx->curfn != 0);
            return LLVMBuildIntToPtr(builder, val, neededtype, llvmgen_temp(gctx));
        }
    }

    // XXX I don't think this would ever happen
    return (constcast ? LLVMConstBitCast(val, neededtype)
            : LLVMBuildBitCast(builder, val, neededtype, llvmgen_temp(gctx)));

} /* llvmgen_cast_trunc_ext */

/*
 * optlevel_handler
 *
 * Handler for the OPTLEVEL=<n> switch.  Levels 0 through 3 are supported.
 */
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
 *
 * Sets up the code generation context for a module.  Called by
 * the front end just after parsing a module declaration, but before
 * processing the module expression.
 *
 * This function creates the module in LLVM, sets up the function
 * pass manager, and, if optimization is enabled, sets up the
 * optimization passes.
 *
 * XXX Needs further work.  MAIN is not handled, for instance.
 */
int
gencode_module_begin (gencodectx_t gctx, name_t *modnp)
{
    char *dl, *triple;
#if 0 // XXX later
    char module_header[256];
    int headerlen;
    strdesc_t *str;
#endif // XXX later

    gctx->modnp = modnp;
    gctx->module = LLVMModuleCreateWithNameInContext(name_azstring(modnp), gctx->llvmctx);

    if (gctx->module == 0) {
        return 0;
    }
    gctx->passmgr = LLVMCreateFunctionPassManagerForModule(gctx->module);
    if (gctx->passmgr == 0) {
        LLVMDisposeModule(gctx->module);
        gctx->module = 0;
        return 0;
    }

    triple = LLVMGetTargetMachineTriple(gctx->mctx->target_machine);
    LLVMSetTarget(gctx->module, triple);
    free(triple);
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

#if 0 // XXX later
    headerlen = 0;
    str = modsym_ident(modnp);
    if (str != 0) {
        int reslen, len = str->len;
        reslen = snprintf(module_header+headerlen, sizeof(module_header)-headerlen,
                          "\t.ident %-*.*s", len, len, str->ptr);
        if (reslen > 0) headerlen += reslen;
    }
    str = modsym_main(modnp);
    if (str != 0) {
        int reslen, len = str->len;
        if (headerlen > 0) module_header[headerlen++] = '\n';
        reslen = snprintf(module_header+headerlen, sizeof(module_header)-headerlen,
                          "\t.set start,_%-*.*s", len, len, str->ptr);
        if (reslen > 0) headerlen += reslen;
    }
    if (headerlen > 0) {
        module_header[headerlen] = '\0';
        LLVMSetModuleInlineAsm(gctx->module, module_header);
    }
#endif // XXX later

    return 1;

} /* gencode_module_begin */

/*
 * gencode_module_end
 *
 * Performs the final verification on a module, generates the
 * requested output (assembly or object code, and optionally the
 * LLVM IR), then cleans up the LLVM context.
 */
int
gencode_module_end (gencodectx_t gctx, name_t *np)
{
    char *err;

    LLVMVerifyModule(gctx->module, LLVMPrintMessageAction, 0);
    if (gctx->mctx->irdumpfile != 0) {
        err = 0;
        if (LLVMPrintModuleToFile(gctx->module, gctx->mctx->irdumpfile, &err)) {
            if (err) { fprintf(stderr, "%s\n", err); LLVMDisposeMessage(err); }
        }
    }
    err = 0;
    if (LLVMTargetMachineEmitToFile(gctx->mctx->target_machine, gctx->module,
                                    gctx->mctx->outfile, gctx->mctx->outputtype, &err)) {
        if (err) { fprintf(stderr, "%s\n", err); LLVMDisposeMessage(err); }
    }
    LLVMDisposePassManager(gctx->passmgr);
    LLVMDisposeModule(gctx->module);

    return 1;

} /* gencode_module_end */

/*
 * gencode_optlevel_set
 *
 * Optimization level setter, invoked through the compiler driver
 * interface.
 */
void
gencode_optlevel_set (gencodectx_t gctx, unsigned int level)
{
    gctx->optlevel = (level & 3);  // valid values are 0, 1, 2, 3

} /* gencode_optlevel_set */

/*
 * gencode_postinit
 *
 * Some of the initialization cannot happen until other parts of the
 * front end have been initialized.  The expression generator initialization
 * (which sets up generators for executable functions and declares BUILTINs)
 * and the OPTLEVEL switch handler fall into this category.
 */
void
gencode_postinit (gencodectx_t gctx)
{
    scopectx_t kwdscope = parser_kwdscope(expr_parse_ctx(gctx->ectx));
    
    llvmgen_expgen_init(gctx);
    switch_special_declare(kwdscope, LEXTYPE_SWITCH_OPTLEVEL, optlevel_handler, gctx);

} /* gencode_postinit */

/*
 * gencode_init
 *
 * Initialization for this module.  Called from expression_init().  Sets
 * up the main context block used throughout the code generator, and calls
 * the symbol generator's initializatino routine.
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

/*
 * gencode_finish
 *
 * Cleanup for code generator.
 */
void
gencode_finish (gencodectx_t gctx)
{
    llvm_btrack_t *bt, *btn;
    llvm_rtntrack_t *rt, *rtn;

    if (gctx == 0) {
        return;
    }

    llvmgen_builtins_finish(gctx->builtinsctx);

    for (bt = gctx->freebts; bt != 0; bt = btn) {
        btn = bt->next;
        free(bt);
    }

    for (rt = gctx->freerts; rt != 0; rt = rtn) {
        rtn = rt->next;
        free(rt);
    }

    free(gctx);

} /* gencode_finish */
