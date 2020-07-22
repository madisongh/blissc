/*
 *++
 * llvm_execfuncgen.c - Executable function generation for LLVM back-end
 *
 *
 * This module generates LLVM IR for control expressions.
 *
 * Functions implemented in this module should include
 * only generic LLVM code; machine-specific functions
 * should go into other modules.  XXX
 *
 * Copyright © 2013-2020, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "llvmgen.h"

#define FUNCGENS \
FUNCGENDEF("MAXA",             gen_MINMAX,          LLVMIntSGT) \
FUNCGENDEF("MINA",             gen_MINMAX,          LLVMIntSLT) \
FUNCGENDEF("MAX",              gen_MINMAX,          LLVMIntSGT) \
FUNCGENDEF("MAXU",             gen_MINMAX,          LLVMIntUGT) \
FUNCGENDEF("MIN",              gen_MINMAX,          LLVMIntSLT) \
FUNCGENDEF("MINU",             gen_MINMAX,          LLVMIntULT) \
FUNCGENDEF("SIGN",             gen_SIGN,            0) \
FUNCGENDEF("ABS",              gen_ABS,             0) \
FUNCGENDEF("CH$ALLOCATION",    gen_ch_allocation,   0) \
FUNCGENDEF("CH$PTR",           gen_ch_pointer,      0) \
FUNCGENDEF("CH$PLUS",          gen_ch_pointer,      0) \
FUNCGENDEF("CH$RCHAR",         gen_ch_rchar,        0) \
FUNCGENDEF("CH$A_RCHAR",       gen_ch_rchar_a,      0) \
FUNCGENDEF("CH$RCHAR_A",       gen_ch_rchar_a,      1) \
FUNCGENDEF("CH$WCHAR",         gen_ch_wchar,        0) \
FUNCGENDEF("CH$A_WCHAR",       gen_ch_wchar_a,      0) \
FUNCGENDEF("CH$WCHAR_A",       gen_ch_wchar_a,      1) \
FUNCGENDEF("CH$MOVE",          gen_ch_move,         0) \
FUNCGENDEF("CH$FILL",          gen_ch_fill,         0) \
FUNCGENDEF("CH$EQL",           gen_ch_compare,      LLVMIntEQ) \
FUNCGENDEF("CH$NEQ",           gen_ch_compare,      LLVMIntNE) \
FUNCGENDEF("CH$LSS",           gen_ch_compare,      LLVMIntULT) \
FUNCGENDEF("CH$LEQ",           gen_ch_compare,      LLVMIntULE) \
FUNCGENDEF("CH$GTR",           gen_ch_compare,      LLVMIntUGT) \
FUNCGENDEF("CH$GEQ",           gen_ch_compare,      LLVMIntUGE) \
FUNCGENDEF("CH$COMPARE",       gen_ch_compare,      -1) \
FUNCGENDEF("CH$FIND_CH",       gen_ch_find,         0) \
FUNCGENDEF("CH$FIND_NOT_CH",   gen_ch_find,         1) \
FUNCGENDEF("CH$FAIL",          gen_ch_fail,         0) \
FUNCGENDEF("CH$FIND_SUB",      gen_ch_findsub,      0) \
FUNCGENDEF("CH$COPY",          gen_ch_copy,         0) \
FUNCGENDEF("CH$TRANSLATE",     gen_ch_translate,    0)

#define FUNCGENDEF(s_, f_, c_) \
    static LLVMValueRef f_ (gencodectx_t gctx, void *fctx, \
                            expr_node_t *node, LLVMTypeRef neededtype);
FUNCGENS
#undef FUNCGENDEF

#define FUNCGENDEF(s_, f_, c_) { s_, f_, (void *)(c_) },
llvm_execfuncgen_t gentable[] = {
    FUNCGENS
};
#undef FUNCGENDEF

/*
 * gen_execfunc
 *
 * Dispatcher for the code generation functions.
 */
static LLVMValueRef
gen_execfunc (gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    funcdef_t *fd = name_extraspace(expr_func_name(exp));
    llvmgen_execfunc_fn realfnptr = (llvmgen_execfunc_fn)(fd->generator);

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
 * into the main expression-generation dispatcher, and sets
 * up the internal dispatcher for each function we support.
 *
 * The initialization call for setting up the generators
 * for the machine-specific executable functions (typically
 * BUILTINs) is also invoked here.
 */
void
llvmgen_execfuncgen_init (gencodectx_t gctx)
{
    scopectx_t kwdscope = parser_kwdscope(expr_parse_ctx(gctx->ectx));
    int addr_signed = machine_addr_signed(gctx->mach);
    unsigned int i;

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
            fd->genfnctx = (void *)(intptr_t)(addr_signed ? LLVMIntSGT : LLVMIntUGT);
        } else if (i == 1) {
            fd->genfnctx = (void *)(intptr_t)(addr_signed ? LLVMIntSLT : LLVMIntULT);
        } else {
            fd->genfnctx = gentable[i].fctx;
        }
    }
    gctx->builtinsctx = llvmgen_builtins_init(gctx, kwdscope);
    llvmgen_expgen_register(gctx, EXPTYPE_EXECFUN, gen_execfunc);

} /* llvmgen_execfuncgen_init */

/*
 * gen_SIGN
 *
 * Generates code for the SIGN standard function.
 */
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

/*
 * gen_ABS
 *
 * Generates code for the ABS standard function.
 */
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

} /* gen_ABS */

/*
 * gen_MINMAX
 *
 * Generates code for the MIN and MAX families
 * of standard functions.
 */
static LLVMValueRef
gen_MINMAX (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    unsigned int bpval = machine_scalar_bits(gctx->mach);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMIntPredicate pred = (LLVMIntPredicate) ctx;
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMTypeRef inttype = LLVMIntTypeInContext(gctx->llvmctx, bpval);
    LLVMValueRef val, cmpval, test;
    expr_node_t *arg;

    arg = exprseq_head(args);
    val = llvmgen_expression(gctx, arg, inttype);
    for (arg = arg->tq_next; arg != 0; arg = arg->tq_next) {
        cmpval = llvmgen_expression(gctx, arg, inttype);
        test = LLVMBuildICmp(builder, pred, val, cmpval, llvmgen_temp(gctx));
        val = LLVMBuildSelect(builder, test, val, cmpval, llvmgen_temp(gctx));
    }

    return llvmgen_adjustval(gctx, val, neededtype,
                             (pred == LLVMIntSLT || pred == LLVMIntSGT));

} /* gen_MINMAX */

/*
 * gen_ch_allocation
 *
 * Generates CH$ALLOCATION code.
 * Note that we only support one character size here -- byte -- so we
 * rely on the frontend validating the character size parameter, if it was
 * specified.
 */
static LLVMValueRef
gen_ch_allocation (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef len, result;
    expr_node_t *arg;

    arg = exprseq_head(args);
    len = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    result = LLVMBuildMul(builder, len, LLVMConstInt(gctx->fullwordtype, 8, 0), llvmgen_temp(gctx));
    result = LLVMBuildAdd(builder, result,
                          LLVMConstInt(gctx->fullwordtype, machine_scalar_bits(gctx->mach)-1, 0),
                          llvmgen_temp(gctx));
    result = LLVMBuildUDiv(builder, result,
                           LLVMConstInt(gctx->fullwordtype, machine_scalar_bits(gctx->mach), 0),
                           llvmgen_temp(gctx));
    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_allocation */

/*
 * gen_ch_pointer
 *
 * Generates CH$PTR code.  Also handles CH$PLUS, since we don't
 * bother checking the third argument to CH$PTR.
 */
static LLVMValueRef
gen_ch_pointer (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef result;
    expr_node_t *arg;

    arg = exprseq_head(args);
    result = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    if (arg->tq_next != 0) {
        LLVMValueRef offset;
        arg = arg->tq_next;
        if (expr_type(arg) == EXPTYPE_PRIM_LIT) {
            if (expr_litval(arg) != 0) {
                offset = LLVMConstInt(gctx->fullwordtype, (unsigned long long int) expr_litval(arg), 0);
                if (LLVMIsConstant(result)) {
                    result = LLVMConstGEP(offset, &offset, 1);
                } else {
                    result = LLVMBuildGEP(builder, result, &offset, 1, llvmgen_temp(gctx));
                }
            }
        } else {
            offset = llvmgen_expression(gctx, arg, gctx->fullwordtype);
            result = LLVMBuildPtrToInt(builder, result, gctx->fullwordtype, llvmgen_temp(gctx));
            result= LLVMBuildAdd(builder, result, offset, llvmgen_temp(gctx));
        }

    }

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_pointer */

/*
 * gen_ch_rchar
 *
 * Generates CH$RCHAR.
 */
static LLVMValueRef
gen_ch_rchar (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef addr, result;
    expr_node_t *arg;

    arg = exprseq_head(args);
    addr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    result = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_rchar */

/*
 * gen_ch_rchar_a
 *
 * Generates CH$RCHAR_A and CH$A_RCHAR.
 */
static LLVMValueRef
gen_ch_rchar_a (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    int postincrement = (int)(intptr_t) ctx;
    LLVMValueRef one = LLVMConstInt(gctx->fullwordtype, 1, 0);
    LLVMValueRef addraddr, addr, result;
    expr_node_t *arg;

    arg = exprseq_head(args);
    addraddr = llvmgen_expression(gctx, arg, LLVMPointerType(gctx->unitptrtype, 0));
    addr = LLVMBuildLoad(builder, addraddr, llvmgen_temp(gctx));
    if (!postincrement) {
        LLVMValueRef tmp = LLVMBuildGEP(builder, addr, &one, 1, llvmgen_temp(gctx));
        LLVMBuildStore(builder, tmp, addraddr);
        addr = tmp;
    }
    result = LLVMBuildLoad(builder, addr, llvmgen_temp(gctx));
    if (postincrement) {
        LLVMValueRef tmp = LLVMBuildGEP(builder, addr, &one, 1, llvmgen_temp(gctx));
        LLVMBuildStore(builder, tmp, addraddr);
    }

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_rchar_a */

/*
 * gen_ch_wchar
 *
 * Generates CH$WCHAR.
 */
static LLVMValueRef
gen_ch_wchar (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef ch, addr;
    expr_node_t *arg;

    arg = exprseq_head(args);
    ch = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    addr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    LLVMBuildStore(builder, ch, addr);

    if (neededtype != 0) {
        return LLVMConstNull(neededtype);
    }

    return 0;

} /* gen_ch_wchar */

/*
 * gen_ch_wchar_a
 *
 * Generates CH$WCHAR_A and CH$A_WCHAR.
 */
static LLVMValueRef
gen_ch_wchar_a (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    int postincrement = (int)(intptr_t) ctx;
    LLVMValueRef one = LLVMConstInt(gctx->fullwordtype, 1, 0);
    LLVMValueRef ch, addraddr, addr;
    expr_node_t *arg;

    arg = exprseq_head(args);
    ch = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    addraddr = llvmgen_expression(gctx, arg, LLVMPointerType(gctx->unitptrtype, 0));
    addr = LLVMBuildLoad(builder, addraddr, llvmgen_temp(gctx));
    if (!postincrement) {
        LLVMValueRef tmp = LLVMBuildGEP(builder, addr, &one, 1, llvmgen_temp(gctx));
        LLVMBuildStore(builder, tmp, addraddr);
        addr = tmp;
    }
    LLVMBuildStore(builder, ch, addr);
    if (postincrement) {
        LLVMValueRef tmp = LLVMBuildGEP(builder, addr, &one, 1, llvmgen_temp(gctx));
        LLVMBuildStore(builder, tmp, addraddr);
    }

    if (neededtype != 0) {
        return LLVMConstNull(neededtype);
    }

    return 0;

} /* gen_ch_wchar_a */

/*
 * gen_ch_move
 *
 * CH$MOVE(len, src, dst)
 * Returns: dst + len
 *
 * For x86, the MOVSB builtin provides this function.
 */
static LLVMValueRef
gen_ch_move (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{

    return llvmgen_builtinfunc(gctx, "MOVSB", exp, neededtype);

} /* gen_ch_move */

/*
 * gen_ch_fill
 *
 * CH$FILL(chr, len, dst)
 * Returns: dst + len
 *
 * For x86, the STOSB builtin provides this function.
 */
static LLVMValueRef
gen_ch_fill (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    return llvmgen_builtinfunc(gctx, "STOSB", exp, neededtype);

} /* gen_ch_fill */

/*
 * gen_ch_compare
 *
 * CH${EQL,NEQ,LSS,GTR,LEQ,GEQ,COMPARE}(len1, ptr1, len2, ptr2, padchr)
 * Returns:
 * for CH${EQL,NEQ,LSS,GTR,LEQ,GEQ}, 1 if true, 0 if false
 * for CH$COMPARE: -1 if less, 0 if equal, 1 if greater
 *
 * On x86, uses CMPSB to compare the string lengths and a form of
 * SCASB to compare against the padding character when the lengths
 * are not the same.
 */
static LLVMValueRef
gen_ch_compare (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef eqchk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef lenchk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef str2longer = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef str1longer = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    int do_general_compare = ((intptr_t) ctx == -1);
    expr_node_t *arg;
    LLVMIntPredicate pred;
    LLVMValueRef argvals[3], t0, t1, bound;
    LLVMValueRef len1, len2, ptr1, ptr2, fill, result, v;

    arg = exprseq_head(args);
    len1 = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    argvals[1] = ptr1 = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    arg = arg->tq_next;
    len2 = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    argvals[2] = ptr2 = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    arg = arg->tq_next;
    fill = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    t0 = LLVMBuildICmp(builder, LLVMIntULE, len1, len2, llvmgen_temp(gctx));
    t1 = LLVMBuildICmp(builder, LLVMIntEQ, len1, len2, llvmgen_temp(gctx));
    argvals[0] = bound = LLVMBuildSelect(builder, t0, len1, len2, llvmgen_temp(gctx));
    result = llvmgen_asminstr(gctx, "CMPSB", argvals, 3);
    v = LLVMBuildSwitch(builder, result, exitblk, 1);
    LLVMAddCase(v, LLVMConstNull(gctx->fullwordtype), eqchk);
    llvmgen_btrack_update_phi(gctx, bt, LLVMGetInsertBlock(builder), result);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, eqchk);
    LLVMBuildCondBr(builder, t1, exitblk, lenchk);
    llvmgen_btrack_update_phi(gctx, bt, LLVMGetInsertBlock(builder), result);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, lenchk);
    LLVMBuildCondBr(builder, t0, str2longer, str1longer);

    LLVMPositionBuilderAtEnd(builder, str2longer);
    argvals[0] = LLVMBuildSub(builder, len2, bound, llvmgen_temp(gctx));
    argvals[1] = LLVMBuildGEP(builder, ptr2, &bound, 1, llvmgen_temp(gctx));
    argvals[2] = fill;
    result = llvmgen_asminstr(gctx, "SCASB_CMP", argvals, 3);
    llvmgen_btrack_update(gctx, bt, result);

    LLVMPositionBuilderAtEnd(builder, str1longer);
    argvals[0] = LLVMBuildSub(builder, len1, bound, llvmgen_temp(gctx));
    argvals[1] = LLVMBuildGEP(builder, ptr1, &bound, 1, llvmgen_temp(gctx));
    argvals[2] = fill;
    result = llvmgen_asminstr(gctx, "SCASB_CMP", argvals, 3);
    llvmgen_btrack_update(gctx, bt, result);

    result = llvmgen_btrack_finalize(gctx, bt, gctx->fullwordtype);
    if (!do_general_compare) {
        pred = (LLVMIntPredicate)(intptr_t)ctx;
        result = LLVMBuildICmp(builder, pred, result, LLVMConstNull(gctx->fullwordtype), llvmgen_temp(gctx));
    }

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_compare */

/*
 * gen_ch_find
 *
 * CH$FIND_CH(len, ptr, char), CH$FIND_NOT_CH(len, ptr, char)
 * Returns: pointer to matching (FIND) or not-matching (FIND_NOT) character,
 * or null pointer on failure.
 *
 * On x86, uses SCASB.
 */
static LLVMValueRef
gen_ch_find (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{

    return llvmgen_builtinfunc(gctx, (ctx == 0 ? "SCASB_REPNE" : "SCASB_REPE"), exp, neededtype);


} /* gen_ch_find */

/*
 * gen_ch_fail
 *
 * CH$FAIL(ptr)
 * Returns: 1 if pointer is null, 0 if non-null
 */
static LLVMValueRef
gen_ch_fail (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBuilderRef builder = gctx->curfn->builder;
    exprseq_t *args = expr_func_arglist(exp);
    LLVMValueRef result, arg;

    arg = llvmgen_expression(gctx, exprseq_head(args), gctx->fullwordtype);
    result = LLVMBuildIsNull(builder, arg, llvmgen_temp(gctx));

    return llvmgen_adjustval(gctx, result, neededtype, 0);

} /* gen_ch_fail */

/*
 * gen_ch_findsub
 *
 * CH$FIND_SUB(strlen, strptr, patlen, patptr)
 * Returns: pointer to start of match, or null pointer on failure
 *
 * Generates code for CH$FIND_SUB.  The algorithm is:
 * 1. If the pattern length is zero, return the start of the string.
 * 2. While the string length >= pattern length:
 * 2a. Scan for the first character of the pattern in the string.
 * 2b. If not found, return failure.
 * 2c. Calculate the remaining length from the start point.
 * 2d. If the remaining length < pattern length, return failure.
 * 2e. Do the comparison with the pattern from that start point.
 * 2f. If a match, return the found start point.
 * 2g. Set the current string pointer to one past the start point.
 * 2h. Set the current string length to (remaining length)-1.
 * 2i. Iterate.
 * 3. If the loop exits, return failure.
 *
 * XXX This is a fair amount of code.  Might make sense to shift this
 * to a runtime support routine instead of inlining.
 */
static LLVMValueRef
gen_ch_findsub (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef findblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef lenchkblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef cmpblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef updblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef one = LLVMConstInt(gctx->fullwordtype, 1, 1);
    expr_node_t *arg;
    LLVMBasicBlockRef curblk;
    LLVMValueRef argvals[3], test, curptr, curlen, remain;
    LLVMValueRef slen, sptr, safter, plen, pptr, result, v, pfirst;

    arg = exprseq_head(args);
    slen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    sptr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    safter = LLVMBuildAdd(builder, llvmgen_adjustval(gctx, sptr, gctx->fullwordtype, 1),
                          slen, llvmgen_temp(gctx));
    arg = arg->tq_next;
    plen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    pptr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    pfirst = LLVMBuildLoad(builder, pptr, llvmgen_temp(gctx));

    test = LLVMBuildIsNull(builder, plen, llvmgen_temp(gctx));
    curblk = LLVMGetInsertBlock(builder);
    LLVMBuildCondBr(builder, test, exitblk, loopblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, sptr);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, loopblk);
    curptr = LLVMBuildPhi(builder, gctx->unitptrtype, llvmgen_temp(gctx));
    LLVMAddIncoming(curptr, &sptr, &curblk, 1);
    curlen = LLVMBuildPhi(builder, gctx->fullwordtype, llvmgen_temp(gctx));
    LLVMAddIncoming(curlen, &slen, &curblk, 1);
    test = LLVMBuildICmp(builder, LLVMIntSGT, curlen, plen, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, findblk, exitblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, LLVMConstNull(gctx->unitptrtype));
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, findblk);
    argvals[0] = curlen;
    argvals[1] = curptr;
    argvals[2] = pfirst;
    result = llvmgen_asminstr(gctx, "SCASB_REPNE", argvals, 3);
    test = LLVMBuildIsNull(builder, result, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, exitblk, lenchkblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, result);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, lenchkblk);
    remain = LLVMBuildSub(builder, safter,
                          llvmgen_adjustval(gctx, result, gctx->fullwordtype, 1),
                          llvmgen_temp(gctx));
    test = LLVMBuildICmp(builder, LLVMIntSLT, remain, plen, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, exitblk, cmpblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, LLVMConstNull(gctx->unitptrtype));
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, cmpblk);
    argvals[0] = plen;
    argvals[1] = pptr;
    argvals[2] = result;
    v = llvmgen_asminstr(gctx, "CMPSB", argvals, 3);
    test = LLVMBuildIsNull(builder, v, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, exitblk, updblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, result);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, updblk);
    v = LLVMBuildGEP(builder, result, &one, 1, llvmgen_temp(gctx));
    LLVMAddIncoming(curptr, &v, &updblk, 1);
    v = LLVMBuildSub(builder, remain, one, llvmgen_temp(gctx));
    LLVMAddIncoming(curlen, &v, &updblk, 1);
    LLVMBuildBr(builder, loopblk);

    result = llvmgen_btrack_finalize(gctx, bt, gctx->unitptrtype);
    return llvmgen_adjustval(gctx, result, neededtype, 0);
    
} /* gen_ch_findsub */

/*
 * gen_ch_copy
 *
 * CH$COPY(src1len, src1ptr, ..., fill, dstlen, dstptr)
 * Returns: (dstptr+dstlen)
 *
 * Generates code for CH$COPY.  Algorithm:
 * 1. Validate the argument count - the front end only checks for
 * an argument count >= 5
 * 2. Set curlen = dstlen, curptr = dstptr
 * 3. Iterate through the source argument pairs (slen, sptr) while curlen > 0:
 * 3a. If slen > curlen, slen = curlen
 * 3b. curptr = CH$MOVE(slen, sptr, dptr)
 * 3c. curptr = CH$PLUS(curptr, slen), curlen -= slen
 * 3d. Iterate.
 * 4. If curlen > 0 (ran out of source arguments): CH$FILL(curlen,curptr,fill)
 *
 * XXX This is a fair amount of code.  Might make sense to shift this
 * to a runtime support routine instead of inlining.
 */
static LLVMValueRef
gen_ch_copy (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef fillblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    exprseq_t *args = expr_func_arglist(exp);
    unsigned int argcnt = exprseq_length(args);
    LLVMBuilderRef builder = gctx->curfn->builder;
    expr_node_t *arg;
    LLVMValueRef argvals[3], test, dstptr, dstlen;
    LLVMValueRef srclen, result, fill;
    unsigned int i;

    // Must be an odd number of arguments (the minimum of 5 check is
    // performed in the front end)
    if ((argcnt % 2) != 1) {
        log_signal(parser_logctx(expr_parse_ctx(gctx->ectx)), expr_textpos(exp),
                   STC__INSFUNARG, name_string(expr_func_name(exp)));
        return (neededtype == 0 ? 0 : LLVMConstNull(neededtype));
    }
    // Get to the 'fill', 'dlen', and 'dptr' args, which are the last 3
    for (arg = exprseq_head(args), i = 0; i < argcnt-3; arg = arg->tq_next, i++);
    fill = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    dstlen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    dstptr = llvmgen_expression(gctx, arg, gctx->unitptrtype);

    for (arg = exprseq_head(args), i = 0; i < argcnt-3;
         arg = arg->tq_next->tq_next, i += 2) {
        LLVMBasicBlockRef domove = LLVMInsertBasicBlockInContext(gctx->llvmctx, fillblk, llvmgen_label(gctx));
        test = LLVMBuildIsNull(builder, dstlen, llvmgen_temp(gctx));
        LLVMBuildCondBr(builder, test, exitblk, domove);
        llvmgen_btrack_update_phi(gctx, bt, 0, dstptr);
        llvmgen_btrack_update_brcount(gctx, bt);

        LLVMPositionBuilderAtEnd(builder, domove);
        srclen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
        argvals[1] = llvmgen_expression(gctx, arg->tq_next, gctx->unitptrtype);
        test = LLVMBuildICmp(builder, LLVMIntSLT, dstlen, srclen, llvmgen_temp(gctx));
        argvals[0] = LLVMBuildSelect(builder, test, dstlen, srclen, llvmgen_temp(gctx));
        argvals[2] = dstptr;
        dstptr = llvmgen_asminstr(gctx, "MOVSB", argvals, 3);
        dstlen = LLVMBuildSub(builder, dstlen, argvals[0], llvmgen_temp(gctx));
    }

    test = LLVMBuildIsNull(builder, dstlen, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, exitblk, fillblk);
    llvmgen_btrack_update_phi(gctx, bt, 0, dstptr);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, fillblk);
    argvals[0] = fill;
    argvals[1] = dstlen;
    argvals[2] = dstptr;
    dstptr = llvmgen_asminstr(gctx, "STOSB", argvals, 3);
    llvmgen_btrack_update(gctx, bt, dstptr);

    result = llvmgen_btrack_finalize(gctx, bt, gctx->unitptrtype);
    return llvmgen_adjustval(gctx, result, neededtype, 0);
    
} /* gen_ch_copy */

/*
 * gen_ch_translate
 *
 * CH$TRANSLATE(table, slen, sptr, fill, dlen, dptr)
 *
 * Generates code for CH$TRANSLATE, walking through the
 * source string and applying the translation one character
 * at a time.
 */
static LLVMValueRef
gen_ch_translate (gencodectx_t gctx, void *ctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    LLVMBasicBlockRef exitblk = llvmgen_exitblock_create(gctx, 0);
    LLVMBasicBlockRef loopblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef loopbody = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef postloop = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    LLVMBasicBlockRef fillblk = LLVMInsertBasicBlockInContext(gctx->llvmctx, exitblk, llvmgen_label(gctx));
    llvm_btrack_t *bt = llvmgen_btrack_create(gctx, exitblk);
    exprseq_t *args = expr_func_arglist(exp);
    LLVMBuilderRef builder = gctx->curfn->builder;
    LLVMValueRef zero = LLVMConstNull(gctx->fullwordtype);
    LLVMValueRef one  = LLVMConstInt(gctx->fullwordtype, 1, 0);
    expr_node_t *arg;
    LLVMBasicBlockRef curblk;
    LLVMValueRef argvals[3], test, loopcount, loopphi, offphi, chr;
    LLVMValueRef transtab, slen, sptr, dlen, dptr, fill, result, v;

    arg = exprseq_head(args);
    transtab = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    arg = arg->tq_next;
    slen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    sptr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    arg = arg->tq_next;
    fill = llvmgen_expression(gctx, arg, LLVMInt8TypeInContext(gctx->llvmctx));
    arg = arg->tq_next;
    dlen = llvmgen_expression(gctx, arg, gctx->fullwordtype);
    arg = arg->tq_next;
    dptr = llvmgen_expression(gctx, arg, gctx->unitptrtype);
    test = LLVMBuildICmp(builder, LLVMIntULE, slen, dlen, llvmgen_temp(gctx));
    loopcount = LLVMBuildSelect(builder, test, slen, dlen, llvmgen_temp(gctx));
    curblk = LLVMGetInsertBlock(builder);
    LLVMBuildBr(builder, loopblk);

    LLVMPositionBuilderAtEnd(builder, loopblk);
    loopphi = LLVMBuildPhi(builder, gctx->fullwordtype, llvmgen_temp(gctx));
    LLVMAddIncoming(loopphi, &loopcount, &curblk, 1);
    offphi = LLVMBuildPhi(builder, gctx->fullwordtype, llvmgen_temp(gctx));
    LLVMAddIncoming(offphi, &zero, &curblk, 1);
    test = LLVMBuildIsNull(builder, loopphi, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, postloop, loopbody);

    LLVMPositionBuilderAtEnd(builder, loopbody);
    sptr = LLVMBuildGEP(builder, sptr, &offphi, 1, llvmgen_temp(gctx));
    chr = LLVMBuildLoad(builder, sptr, llvmgen_temp(gctx));
    v = LLVMBuildGEP(builder, transtab, &chr, 1, llvmgen_temp(gctx));
    chr = LLVMBuildLoad(builder, v, llvmgen_temp(gctx));
    v = LLVMBuildGEP(builder, dptr, &offphi, 1, llvmgen_temp(gctx));
    LLVMBuildStore(builder, chr, v);
    v = LLVMBuildSub(builder, loopphi, one, llvmgen_temp(gctx));
    LLVMAddIncoming(loopphi, &v, &loopbody, 1);
    v = LLVMBuildAdd(builder, offphi, one, llvmgen_temp(gctx));
    LLVMAddIncoming(offphi, &v, &loopbody, 1);
    LLVMBuildBr(builder, loopblk);

    LLVMPositionBuilderAtEnd(builder, postloop);
    dptr = LLVMBuildGEP(builder, dptr, &loopcount, 1, llvmgen_temp(gctx));
    test = LLVMBuildICmp(builder, LLVMIntULT, loopcount, dlen, llvmgen_temp(gctx));
    LLVMBuildCondBr(builder, test, fillblk, exitblk);
    llvmgen_btrack_update_phi(gctx, bt, LLVMGetInsertBlock(builder), dptr);
    llvmgen_btrack_update_brcount(gctx, bt);

    LLVMPositionBuilderAtEnd(builder, fillblk);
    argvals[0] = fill;
    argvals[1] = LLVMBuildSub(builder, dlen, offphi, llvmgen_temp(gctx));
    argvals[2] = dptr;
    dptr = llvmgen_asminstr(gctx, "STOSB", argvals, 3);
    llvmgen_btrack_update(gctx, bt, dptr);

    result = llvmgen_btrack_finalize(gctx, bt, gctx->unitptrtype);
    return llvmgen_adjustval(gctx, result, neededtype, 0);
    
} /* gen_ch_translate */
