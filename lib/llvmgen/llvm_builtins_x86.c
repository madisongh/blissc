/*
 *++
 * llvm_builtins_x86.c - Builtins for x86 processors.
 *
 *
 * This module generates LLVM IR for X86 assembly instructions.
 *
 * Copyright © 2013-2020, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "llvmgen.h"

// Structure for tracking the inline assembly we generate for machine-specific
// built-ins.
//
// The 'arginfo' field is a character string: the first character codes
// the return type; subsequent characters code argument types and "extra" arguments
// that are added, where needed, to adjust results. Codes are:
// V=void (only for return type), P=pointer, F=fullword, or 1,2,4,8 = int size in bytes
// X=extra arg to be inserted, assumed type is fullword, followed by value in parentheses
//
// The 'argcount' field is the number of arguments expected when parsing the source.
// The 'actcount' field is the actual number of pseudo-arguments used in the LLVM code,
// which includes the 'X' extra arguments.
//
struct asminfo_s {
    const char *        name;
    const char *        instr;
    const char *        reguse;
    unsigned int        argcount;
    unsigned int        actcount;
    const char *        arginfo;
    unsigned int        flags;
};

#define ASMGENS64 \
ASMGENDEF("MOVSB", "cld;rep;movsb", "={rdi},{rcx},{rsi},{rdi},~{dflag},~{rcx},~{rsi}", \
                                                                         3, 3, "PFPP", LLVMGEN_M_ASM_SIDEEFFECT) \
ASMGENDEF("STOSB", "cld;rep;stosb", "={rdi},{al},{rcx},{rdi},~{dflag},~{rcx}", \
                                                                         3, 3, "P1FP", LLVMGEN_M_ASM_SIDEEFFECT) \
ASMGENDEF("CMPSB", "cld;repe;cmpsb;cmovb %rax,%rcx;cmova %rdx,%rcx", \
                   "={rcx},{rcx},{rsi},{rdi},{rax},{rdx},~{dflag},~{rcx},~{rdi},~{rsi}", \
                                                                         3, 5, "FFPPX(-1)X(1)", 0) \
ASMGENDEF("SCASB_REPE", "cld;repe;scasb;cmoveq %rdx,%rdi;subq %rdx,%rdi", \
                        "={rdi},{rcx},{rdi},{al},{rdx},~{dflag},~{rcx}", 3, 4, "PFP1X(1)", 0) \
ASMGENDEF("SCASB_REPNE", "cld;repne;scasb;cmovne %rdx,%rdi;subq %rdx,%rdi", \
                         "={rdi},{rcx},{rdi},{al},{rdx},~{dflag},~{rcx}", \
                                                                         3, 4, "PFP1X(1)", 0) \
ASMGENDEF("SCASB_CMP", "cld;repe;scasb;cmovb %rbx,%rcx;cmova %rdx,%rcx", \
                       "={rcx},{rcx},{rdi},{al},{rbx},{rdx},~{dflag},~{rcx},~{rdi}", \
                                                                         3, 5, "FFP1X(-1)X(1)", 0)

#define ASMGENS32 \
ASMGENDEF("MOVSB", "cld;rep;movsb", "={edi},{ecx},{esi},{edi},~{dflag},~{ecx},~{esi}", \
                                                                         3, 3, "PFPP", LLVMGEN_M_ASM_SIDEEFFECT) \
ASMGENDEF("STOSB", "cld;rep;stosb", "={edi},{al},{ecx},{edi},~{dflag},~{ecx}", \
                                                                         3, 3, "P1FP", LLVMGEN_M_ASM_SIDEEFFECT) \
ASMGENDEF("CMPSB", "cld;repe;cmpsb;cmovb %eax,%ecx;cmova %edx,%ecx", \
                   "={ecx},{ecx},{esi},{edi},{eax},{edx},~{dflag},~{ecx},~{edi},~{esi}", \
                                                                         3, 5, "FFPPX(-1)X(1)", 0) \
ASMGENDEF("SCASB_REPE", "cld;repe;scasb;cmovel %edx,%edi;subl %edx,%edi", \
                        "={edi},{ecx},{edi},{al},{edx},~{dflag},~{ecx}", 3, 4, "PFP1X(1)", 0) \
ASMGENDEF("SCASB_REPNE", "cld;repne;scasb;cmovne %edx,%edi;subl %edx,%edi", \
                         "={edi},{ecx},{edi},{al},{edx},~{dflag},~{ecx}", \
                                                                         3, 4, "PFP1X(1)", 0) \
ASMGENDEF("SCASB_CMP", "cld;repe;scasb;cmovb %ebx,%ecx;cmova %edx,%ecx", \
                       "={ecx},{ecx},{edi},{al},{ebx},{edx},~{dflag},~{ecx},~{edi}", \
                                                                         3, 5, "FFP1X(-1)X(1)", 0)


#define ASMGENDEF(n_, i_, r_, ac1_, ac2_, ai_, f_) { n_, i_, r_, ac1_, ac2_, ai_, f_ },
struct asminfo_s asmtable64[] = {
    ASMGENS64
};
struct asminfo_s asmtable32[] = {
    ASMGENS32
};
#undef ASMGENDEF


/*
 * decode type
 *
 * Called to iterate through the 'arginfo' string, building up the
 * LLVM function type definition for an assembly instruction, along
 * with the values for any hidden extra arguments.
 */
static const char *
decode_type (gencodectx_t gctx, const char *arginfo, LLVMTypeRef *typep, LLVMValueRef *xargp)
{
    const char *cp = arginfo;

    switch (*cp) {
        case 'V':
            *typep = LLVMVoidTypeInContext(gctx->llvmctx);
            if (xargp != 0) *xargp = 0;
            cp += 1;
            break;
        case 'F':
            *typep = gctx->fullwordtype;
            if (xargp != 0) *xargp = 0;
            cp += 1;
            break;
        case 'P':
            *typep = gctx->unitptrtype;
            if (xargp != 0) *xargp = 0;
            cp += 1;
            break;
        case '1':
        case '2':
        case '4':
        case '8':
            *typep = LLVMIntTypeInContext(gctx->llvmctx, (*cp - '0') * 8U);
            if (xargp != 0) *xargp = 0;
            cp += 1;
            break;
        case 'X': {
            unsigned long val = 0;
            int negate = 0;
            *typep = gctx->fullwordtype;
            cp += 1;
            if (*cp != '(' || xargp == 0) {
                expr_signal(gctx->ectx, STC__INTCMPERR, "decode_type[1]");
            }
            cp += 1;
            if (*cp == '-') {
                negate = 1;
                cp += 1;
            }
            while (*cp != '\0' && *cp != ')') {
                val = val * 10 + (*cp - '0');
                cp += 1;
            }
            if (negate) {
                long signedval = (long) val;
                val = (unsigned long) -signedval;
            }
            if (xargp != 0) *xargp = LLVMConstInt(*typep, val, 1);
            if (*cp != '\0') cp += 1;
            break;
        }
        default:
            expr_signal(gctx->ectx, STC__INTCMPERR, "decode_type[2]");
            break;
    }

    return cp;

} /* decode_type */

/*
 * gen_asminstr_data
 *
 * Creates the LLVM constructs representing an assembly instruction.
 */
static void
gen_asminstr_data (gencodectx_t gctx, llvm_asminstr_t *asmp)
{
    struct asminfo_s *ai = asmp->instrinfo;
    unsigned int i;
    const char *cp;

    cp = decode_type(gctx, ai->arginfo, &asmp->rettype, 0);

    i = 0;
    while (*cp != '\0') {
        cp = decode_type(gctx, cp, &asmp->argtypes[i], &asmp->xargs[i]);
        i += 1;
    }

    asmp->functype = LLVMFunctionType(asmp->rettype, asmp->argtypes, asmp->argcount, 0);
    asmp->asminstr = LLVMConstInlineAsm(asmp->functype, ai->instr, ai->reguse,
                                        ((asmp->flags & LLVMGEN_M_ASM_SIDEEFFECT) != 0),
                                        ((asmp->flags & LLVMGEN_M_ASM_ALIGNSTACK) != 0));

} /* gen_asminstr_data */

/*
 * gen_asminstr
 *
 * Generates a call to an assembly instruction.  Used primarily for
 * expanding machine-specific assembly BUILTINs.
 */
LLVMValueRef
gen_asminstr (gencodectx_t gctx, void *fctx, expr_node_t *exp, LLVMTypeRef neededtype)
{
    llvm_asminstr_t *asmp = fctx;
    exprseq_t *args = expr_func_arglist(exp);
    expr_node_t *arg;
    unsigned int i;
    LLVMValueRef argvals[LLVMGEN_K_ASM_MAXARGS], result;

    if (asmp->functype == 0) {
        gen_asminstr_data(gctx, asmp);
    }
    for (i = 0, arg = exprseq_head(args); arg != 0; i++, arg = arg->tq_next) {
        argvals[i] = llvmgen_expression(gctx, arg, asmp->argtypes[i]);
    }
    while (i < asmp->argcount) {
        argvals[i] = asmp->xargs[i];
    i += 1;
    }
    if (LLVMGetTypeKind(asmp->rettype) == LLVMVoidTypeKind) {
        LLVMBuildCall2(gctx->curfn->builder, asmp->functype, asmp->asminstr, argvals, asmp->argcount, "");
        result = (neededtype == 0 ? 0 : LLVMConstNull(neededtype));
    } else {
        result = LLVMBuildCall2(gctx->curfn->builder, asmp->functype, asmp->asminstr, argvals,
                               asmp->argcount, llvmgen_temp(gctx));
        result = llvmgen_adjustval(gctx, result, neededtype, 0);
    }

    return result;

} /* gen_asminstr */

/*
 * llvmgen_builtins_init
 *
 * 1. Sets up a name table for assembly instructions so we can resolve them
 * lazily.
 *
 * 2. Declares BUILTIN executable functions for the assembly instructions.
 */
void *
llvmgen_builtins_init (gencodectx_t gctx, scopectx_t kwdscope)
{
    machinedef_t *mach = gctx->mach;
    int i, numinstr;
    funcdef_t fdef;
    llvm_asminstr_t *asminstrs, *asmp;
    struct asminfo_s *asmtable;
    
    if (machine_scalar_bits(mach) == 64) {
        asmtable = asmtable64;
        numinstr = sizeof(asmtable64)/sizeof(asmtable64[0]);
    } else {
        asmtable = asmtable32;
        numinstr = sizeof(asmtable32)/sizeof(asmtable32[0]);
    }
    asminstrs = malloc(numinstr * sizeof(llvm_asminstr_t));
    memset(asminstrs, 0, numinstr * sizeof(llvm_asminstr_t));

    for (i = 0, asmp = asminstrs; i < numinstr; i++, asmp++) {
        name_t *np;

        memset(&fdef, 0, sizeof(fdef));
        strcpy(fdef.name, asmtable[i].name);
        fdef.namelen = strlen(asmtable[i].name);
        fdef.numargs = asmtable[i].argcount;
        fdef.generator = (execfuncgenerator_fn) gen_asminstr;
        fdef.genfnctx = asmp;
        fdef.flags = FUNC_M_BUILTIN | (asmtable[i].arginfo[0] == 'V' ? FUNC_M_NOVALUE : 0);
        asmp->flags = asmtable[i].flags;
        asmp->argcount = asmtable[i].actcount;
        asmp->instrinfo = &asmtable[i];
        np = execfunc_define(kwdscope, &fdef, 0);
        if (np == 0) {
            expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_builtins_init");
            continue;
        }
    }

    return asminstrs;

} /* llvmgen_builtins_init */

/*
 * llvmgen_builtins_finish
 *
 * Module cleanup.
 */
void
llvmgen_builtins_finish (void *actx)
{
    if (actx != 0) {
        free(actx);
    }

} /* llvmgen_builtins_finish */

/*
 * llvmgen_builtinfunc
 *
 * External API for generating an assembly call.  Used for direct
 * translation of some other executable function expression
 * (e.g., for a CH$ function) into assembly.
 */
LLVMValueRef
llvmgen_builtinfunc (gencodectx_t gctx, const char *name, expr_node_t *exp, LLVMTypeRef neededtype)
{
    llvm_asminstr_t *asmp;
    funcdef_t *fd;
    name_t *np;

    np = name_search_typed_special(parser_kwdscope(expr_parse_ctx(gctx->ectx)),
                                   name, strlen(name), LEXTYPE_NAME_FUNCTION, &fd);
    if (np == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_builtinfunc");
        return 0;
    }
    asmp = fd->genfnctx;
    return gen_asminstr(gctx, asmp, exp, neededtype);

} /* llvmgen_builtinfunc */

/*
 * llvmegen_asminstr
 *
 * External API for generating an assembly call with arguments already in
 * LLVM value form.
 */
LLVMValueRef
llvmgen_asminstr (gencodectx_t gctx, const char *name, LLVMValueRef *arg, unsigned int argcnt)
{
    llvm_asminstr_t *asmp;
    funcdef_t *fd;
    name_t *np;
    LLVMValueRef result, argvals[LLVMGEN_K_ASM_MAXARGS];
    unsigned int i;

    np = name_search_typed_special(parser_kwdscope(expr_parse_ctx(gctx->ectx)),
                                   name, strlen(name), LEXTYPE_NAME_FUNCTION, &fd);
    if (np == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_asminstr");
        return 0;
    }
    asmp = fd->genfnctx;
    if (asmp->functype == 0) {
        gen_asminstr_data(gctx, asmp);
    }
    for (i = 0; i < argcnt; i++) {
        argvals[i] = llvmgen_adjustval(gctx, arg[i], asmp->argtypes[i], 1);
    }
    while (i < asmp->argcount) {
        argvals[i] = asmp->xargs[i];
    i += 1;
    }
    if (LLVMGetTypeKind(asmp->rettype) == LLVMVoidTypeKind) {
        LLVMBuildCall2(gctx->curfn->builder, asmp->functype, asmp->asminstr, argvals, asmp->argcount, "");
        result = 0;
    } else {
        result = LLVMBuildCall2(gctx->curfn->builder, asmp->functype, asmp->asminstr, argvals,
                               asmp->argcount, llvmgen_temp(gctx));
    }

    return result;

} /* llvmgen_asminstr */

/*
 * llvmgen_memcpy
 *
 * Generates a machine-specific equivalent of an LLVM 'memcpy' intrinsic
 * (or close enough). Used internally for data segement initialization.
 */
void
llvmgen_memcpy (gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef src, LLVMValueRef len)
{
    LLVMValueRef arg[3];

    arg[0] = len;
    arg[1] = src;
    arg[2] = dest;
    llvmgen_asminstr(gctx, "MOVSB", arg, 3);

} /* llvmgen_memcpy */

/*
 * llvmgen_memset
 *
 * Generates a machine-specific equivalent of an LLVM 'memset' intrinsic
 * (or close enough). Used internally for data segement initialization.
 */
void
llvmgen_memset (gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef val, LLVMValueRef len)
{

    LLVMValueRef arg[3];
    arg[0] = val;
    arg[1] = len;
    arg[2] = dest;
    llvmgen_asminstr(gctx, "STOSB", arg, 3);

} /* llvmgen_memset */
