/*
 *++
 *	File:			llvm_execfuncgen.c
 *
 *	Abstract:		Executable function generation for LLVM back-end
 *
 *  Module description:
 *
 *       This module generates LLVM IR for control expressions.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		21-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */

#include "llvmgen.h"
#include "blissc/execfuncs.h"

typedef LLVMValueRef (*realgenerator_fn)(gencodectx_t gctx, void *fctx,
                                         expr_node_t *node, LLVMTypeRef neededtype);

#define FUNCGENS \
FUNCGENDEF("MAXA",             gen_MINMAX,          LLVMIntSGT) \
FUNCGENDEF("MINA",             gen_MINMAX,          LLVMIntSLT) \
FUNCGENDEF("MAX",              gen_MINMAX,          LLVMIntSGT) \
FUNCGENDEF("MAXU",             gen_MINMAX,          LLVMIntUGT) \
FUNCGENDEF("MIN",              gen_MINMAX,          LLVMIntSLT) \
FUNCGENDEF("MINU",             gen_MINMAX,          LLVMIntULT) \
FUNCGENDEF("SIGN",             gen_SIGN,            0) \
FUNCGENDEF("ABS",              gen_ABS,             0) \
FUNCGENDEF("CH$MOVE",          gen_chf_move,        0) \
FUNCGENDEF("CH$FILL",          gen_chf_fill,        0)
#if 0
FUNCGENDEF("CH$ALLOCATION",    gen_chf_allocation,  0) \
FUNCGENDEF("CH$SIZE",          gen_chf_size,        0) \
FUNCGENDEF("CH$PTR",           gen_chf_pointer,     0) \
FUNCGENDEF("CH$PLUS",          gen_chf_plus,        0) \
FUNCGENDEF("CH$RCHAR",         gen_chf_rchar,       0) \
FUNCGENDEF("CH$A_RCHAR",       gen_chf_rchar_a,     0) \
FUNCGENDEF("CH$RCHAR_A",       gen_chf_rchar_a,     1) \
FUNCGENDEF("CH$WCHAR",         gen_chf_wchar,       0) \
FUNCGENDEF("CH$A_WCHAR",       gen_chf_wchar_a,     0) \
FUNCGENDEF("CH$WCHAR_A",       gen_chf_wchar_a,     1) \
FUNCGENDEF("CH$COPY",          gen_chf_copy,        0) \
FUNCGENDEF("CH$EQL",           gen_chf_compare,     LLVMIntEQ) \
FUNCGENDEF("CH$NEQ",           gen_chf_compare,     LLVMIntNE) \
FUNCGENDEF("CH$LSS",           gen_chf_compare,     LLVMIntULT) \
FUNCGENDEF("CH$LEQ",           gen_chf_compare,     LLVMIntULE) \
FUNCGENDEF("CH$GTR",           gen_chf_compare,     LLVMIntUGT) \
FUNCGENDEF("CH$GEQ",           gen_chf_compare,     LLVMIntUGE) \
FUNCGENDEF("CH$COMPARE",       gen_chf_compare,     0) \
FUNCGENDEF("CH$FIND_CH",       gen_chf_find,        0) \
FUNCGENDEF("CH$FIND_NOT_CH",   gen_chf_find,        1) \
FUNCGENDEF("CH$FIND_SUB",      gen_chf_findsub,     0) \
FUNCGENDEF("CH$FAIL",          gen_chf_fail,        0) \
FUNCGENDEF("CH$TRANSLATE",     gen_chf_translate,   0)
#endif

struct execfuncgen_s {
    char                    *name;
    realgenerator_fn         func;
    void                    *fctx;
};

#define FUNCGENDEF(s_, f_, c_) \
    static LLVMValueRef f_ (gencodectx_t gctx, void *fctx, \
                            expr_node_t *node, LLVMTypeRef neededtype);
FUNCGENS
#undef FUNCGENDEF

#define FUNCGENDEF(s_, f_, c_) { s_, f_, (void *)(c_) },
struct execfuncgen_s gentable[] = {
    FUNCGENS
};
#undef FUNCGENDEF

static LLVMValueRef
gen_execfunc (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    funcdef_t *fd = name_extraspace(expr_func_name(exp));
    realgenerator_fn realfnptr = (realgenerator_fn)(fd->generator);

    if (realfnptr == 0) {
        unsigned int bpval = machine_scalar_bits(gctx->mach);
        LLVMTypeRef type = (neededtype == 0 ?
                            LLVMIntTypeInContext(gctx->llvmctx, bpval) : neededtype);
        expr_signal(gctx->ectx, STC__INTCMPERR, "gen_execfunc");
        return LLVMConstNull(type);
    }
    return (*realfnptr)(gctx, fd->genfnctx, exp, neededtype);

} /* gen_execfunc */

/*
 * llvmgen_execfuncgen_init
 *
 * Hooks the generators to the executable functions and
 * into the main expression-generation dispatcher.
 */
void
llvmgen_execfuncgen_init (gencodectx_t gctx)
{
    scopectx_t kwdscope = parser_kwdscope(expr_parse_ctx(gctx->ectx));
    int addr_signed = machine_addr_signed(gctx->mach);
    int i;

    for (i = 0; i < sizeof(gentable)/sizeof(gentable[0]); i++) {
        funcdef_t *fd;
        name_t *np;
        np = name_search_typed(kwdscope, gentable[i].name, strlen(gentable[i].name),
                               LEXTYPE_NAME_FUNCTION, &fd);
        if (np == 0) {
            expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_execfuncgen_init");
            continue;
        }
        fd->generator = (execfuncgenerator_fn) gentable[i].func;
        if (i == 0) {
            fd->genfnctx = (void *)(addr_signed ? LLVMIntSGT : LLVMIntUGT);
        } else if (i == 1) {
            fd->genfnctx = (void *)(addr_signed ? LLVMIntSLT : LLVMIntULT);
        } else {
            fd->genfnctx = gentable[i].fctx;
        }
    }
    llvmgen_expgen_register(gctx, EXPTYPE_EXECFUN, gen_execfunc);

} /* llvmgen_execfuncgen_init */

static LLVMValueRef
gen_SIGN (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef posblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef negblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    unsigned int bpval = machine_scalar_bits(gctx->mach);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef val, test;

    if (neededtype == 0) neededtype = LLVMIntTypeInContext(gctx->llvmctx, bpval);
    val = llvmgen_expression(gctx, exprseq_head(args), 0);
    test = LLVMBuildICmp(builder, LLVMIntEQ, val, LLVMConstNull(LLVMTypeOf(val)), llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, exitblk, posblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, LLVMConstNull(neededtype));
    llvmgen_btrack_update_brcount(gctx, bt);
    LLVMPositionBuilderAtEnd(builder, posblk);
    test = LLVMBuildICmp(builder, LLVMIntSLT, val, LLVMConstNull(LLVMTypeOf(val)), llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, negblk, exitblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, LLVMConstInt(neededtype, 1, 0));
    llvmgen_btrack_update_brcount(gctx, bt);
    LLVMPositionBuilderAtEnd(builder, negblk);
    llvmgen_btrack_update(gctx, bt, LLVMConstAllOnes(neededtype));

    return llvmgen_btrack_finalize(gctx, bt, neededtype);

} /* gen_SIGN */

static LLVMValueRef
gen_ABS (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef negblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef val, test, result;

    val = llvmgen_expression(gctx, exprseq_head(args), 0);
    test = LLVMBuildICmp(builder, LLVMIntSLT, val, LLVMConstNull(LLVMTypeOf(val)), llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, negblk, exitblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, val);
    llvmgen_btrack_update_brcount(gctx, bt);
    LLVMPositionBuilderAtEnd(builder, negblk);
    val = LLVMBuildSub(builder, LLVMConstNull(LLVMTypeOf(val)), val, llvmgen_temp(gctx));
    llvmgen_btrack_update(gctx, bt, val);

    result = llvmgen_btrack_finalize(gctx, bt, LLVMTypeOf(val));

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_SIGN */

static LLVMValueRef
gen_MINMAX (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    unsigned int bpval = machine_scalar_bits(gctx->mach);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMIntPredicate pred = (LLVMIntPredicate) ctx;
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMTypeRef inttype = LLVMIntTypeInContext(gctx->llvmctx, bpval);
    LLVMBasicBlockRef here, yesdest, nodest;
    LLVMValueRef val, cmpval, nextphi, test, result;
    expr_node_t *arg;

    arg = exprseq_head(args);
    val = llvmgen_expression(gctx, arg, inttype);
    here = LLVMGetInsertBlock(builder);
    for (arg = arg->tq_next; arg != 0; arg = arg->tq_next) {
        nodest = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
        if (arg->tq_next == 0) {
            yesdest = exitblk;
            nextphi = 0;
        } else {
            yesdest = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
            LLVMPositionBuilderAtEnd(builder, yesdest);
            nextphi = LLVMBuildPhi(builder, inttype, llvmgen_temp(gctx));
            LLVMPositionBuilderAtEnd(builder, here);
        }
        cmpval = llvmgen_expression(gctx, arg, inttype);
        test = LLVMBuildICmp(builder, pred, val, cmpval, llvmgen_temp(gctx));
        LLVMBuildCondBr(builder, test, yesdest, nodest);
        here = LLVMGetInsertBlock(builder);
        if (yesdest == exitblk) {
            llvmgen_btrack_update_phi(gctx, bt, here, val);
            llvmgen_btrack_update_brcount(gctx, bt);
        } else {
            LLVMAddIncoming(nextphi, &val, &here, 1);
        }
        LLVMPositionBuilderAtEnd(builder, nodest);
        if (yesdest == exitblk) {
            llvmgen_btrack_update(gctx, bt, cmpval);
        } else {
            LLVMBuildBr(builder, yesdest);
            LLVMAddIncoming(nextphi, &cmpval, &nodest, 1);
        }
        LLVMPositionBuilderAtEnd(builder, yesdest);
        here = yesdest;
        val = nextphi;
    }

    result = llvmgen_btrack_finalize(gctx, bt, inttype);
    return llvmgen_adjustval(gctx, result, neededtype,
                             (pred == LLVMIntSLT || pred == LLVMIntSGT));

} /* gen_MINMAX */

static LLVMValueRef
gen_chf_move (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef len, src, dst;
    expr_node_t *arg;

    arg = exprseq_head(args);
    len = llvmgen_expression(gctx, arg, LLVMInt32TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    src = llvmgen_expression(gctx, arg, LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0));
    arg = arg->tq_next;
    dst = llvmgen_expression(gctx, arg, LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0));
    llvmgen_memcpy(gctx, dst, src, len);
    if (neededtype == 0) return 0;
    return LLVMConstNull(neededtype);
}

static LLVMValueRef
gen_chf_fill (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef len, fill, dst;
    expr_node_t *arg;

    arg = exprseq_head(args);
    fill = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    len = llvmgen_expression(gctx, arg, LLVMInt32TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    dst = llvmgen_expression(gctx, arg, LLVMPointerType(LLVMInt8TypeInContext(gctx->llvmctx), 0));
    llvmgen_memset(gctx, dst, fill, len);
    if (neededtype == 0) return 0;
    return LLVMConstNull(neededtype);
}
