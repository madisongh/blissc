/*
 *++
 * execfuncs.c - Executable functions.
 *
 * This module implements the dispatch mechanism for
 * handling executable function calls, as well as
 * implementing the standard functions.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "blissc/execfuncs.h"
#include "blissc/expression.h"
#include "blissc/parser.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"
#include "blissc/charfuncs.h"

static int cmplss (long val1, long val2) { return (val1 < val2); }
static int cmplssu (long val1, long val2) {
    return ((unsigned long) val1 < (unsigned long)val2); }
static int cmpgtr (long val1, long val2) { return (val1 > val2); }
static int cmpgtru (long val1, long val2) {
    return ((unsigned long) val1 > (unsigned long) val2); }

static expr_node_t *execfunc_MINMAX(expr_ctx_t ctx, compare_fn fn,
                                    name_t *fnp, exprseq_t *arglist, textpos_t curpos);
static expr_node_t *execfunc_SIGN(expr_ctx_t ctx, compare_fn fn, name_t *fnp,
                                  exprseq_t *arglist, textpos_t curpos);
static expr_node_t *execfunc_ABS(expr_ctx_t ctx, compare_fn fn, name_t *fnp,
                                 exprseq_t *arglist, textpos_t curpos);

static funcdef_t stdfuncs[] = {
    FUNCDEF("MAX",  execfunc_MINMAX, cmpgtr,  2, FUNC_M_VARARGS),
    FUNCDEF("MAXA", execfunc_MINMAX, cmpgtr,  2, FUNC_M_VARARGS),
    FUNCDEF("MAXU", execfunc_MINMAX, cmpgtru, 2, FUNC_M_VARARGS),
    FUNCDEF("MIN",  execfunc_MINMAX, cmplss,  2, FUNC_M_VARARGS),
    FUNCDEF("MINA", execfunc_MINMAX, cmplss,  2, FUNC_M_VARARGS),
    FUNCDEF("MINU", execfunc_MINMAX, cmplssu, 2, FUNC_M_VARARGS),
    FUNCDEF("SIGN", execfunc_SIGN, 0, 1, 0),
    FUNCDEF("ABS",  execfunc_ABS,  0, 1, 0)
};


/*
 * parse_func_args
 *
 * Utility routine for parsing argument lists for functions.
 */
static int
parse_func_args (expr_ctx_t ctx, exprseq_t *arglist)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        return 0;
    }
    while (1) {
        expr_node_t *exp;
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
            exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
        }
        exprseq_instail(arglist, exp);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
        }
    }

    return 1;

} /* parse_func_args */

/*
 * function_bind
 *
 * expr dispatch function for executable functions
 *
 * Parses the argument list and either creates an expression
 * node for the function call or invokes the per-function
 * dispatcher for further processing.
 *
 */
static expr_node_t *
function_bind (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    name_t *np = lexeme_ctx_get(lex);
    textpos_t pos = parser_curpos(expr_parse_ctx(ctx));
    funcdef_t *func;
    exprseq_t args;
    expr_node_t *result = 0;

    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "function_bind");
        return 0;
    }

    func = name_extraspace(np);
    exprseq_init(&args);
    if ((func->flags & FUNC_M_NOPARSE) == 0 && !parse_func_args(ctx, &args)) {
        expr_signal(ctx, STC__SYNTAXERR);
        return 0;
    }

    if (func->handler == 0) {
        if ((func->flags & FUNC_M_NOPARSE) == 0) {
            int varargs = (func->flags & FUNC_M_VARARGS) != 0;
            if ((varargs && exprseq_length(&args) < func->numargs) ||
                (!varargs && exprseq_length(&args) != func->numargs)) {
                expr_signal(ctx, STC__INSFUNARG, name_string(np));
            }
        }
        result = expr_node_alloc(ctx, EXPTYPE_EXECFUN, pos);
        if (result != 0) {
            expr_func_name_set(result, np);
            exprseq_append(expr_func_arglist(result), &args);
            expr_has_value_set(result, (func->flags & FUNC_M_NOVALUE) == 0);
        }
    } else {
        result = (*func->handler)(ctx, func->fn, np, &args, pos);
    }

    if (result == 0) {
        exprseq_free(ctx, &args);
    }

    return result;

} /* function_bind */

/*
 * execfunc_define
 *
 * Entry point for other modules that need to declare
 * executable functions.
 */
name_t *
execfunc_define (scopectx_t scope, funcdef_t *funcdef, textpos_t pos)
{
    namedef_t ndef;
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_FUNCTION;
    ndef.name = funcdef->name;
    ndef.namelen = funcdef->namelen;
    ndef.flags |= (funcdef->flags & FUNC_M_BUILTIN) == 0 ? 0 : NAME_M_BUILTIN;
    return name_declare(scope, &ndef, pos, funcdef, sizeof(funcdef_t), 0);

} /* execfunc_define */

/*
 * execfunc_init
 *
 * Initialization for this module.  Called by expr_init.
 */
void
execfunc_init (expr_ctx_t ctx, scopectx_t scope)
{
    unsigned int i;
    namedef_t ndef;
    nametype_vectors_t fvec;

    memset(&fvec, 0, sizeof(fvec));
    fvec.typesize = sizeof(funcdef_t);
    nametype_dataop_register(expr_namectx(ctx), LEXTYPE_NAME_FUNCTION, &fvec, 0);
    expr_dispatch_register(ctx, LEXTYPE_NAME_FUNCTION, function_bind);

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_FUNCTION;
    for (i = 0; i < sizeof(stdfuncs)/sizeof(stdfuncs[0]); i++) {
        ndef.name = stdfuncs[i].name;
        ndef.namelen = stdfuncs[i].namelen;
        name_declare(scope, &ndef, 0, &stdfuncs[i], sizeof(stdfuncs[i]), 0);
    }

    charfuncs_init(ctx, scope);

} /* execfunc_init */

/*
 * execfunc_MINMAX
 *
 * Implements the MAX{A|U|} and MIN{A|U|} functions, automatically
 * reducing compile-time-constant arguments to the appropriate single
 * value.
 */
static expr_node_t *
execfunc_MINMAX (expr_ctx_t ctx, compare_fn cmpfn, name_t *fnp, exprseq_t *arglist,
                 textpos_t curpos)
{
    machinedef_t *mach = expr_machinedef(ctx);
    strdesc_t *fname = name_string(fnp);
    exprseq_t newargs;
    expr_node_t *arg, *result;
    long curmax;
    int did1 = 0;

    if (fname->len > 3 && fname->ptr[3] == 'A' && !machine_addr_signed(mach)) {
        cmpfn = (cmpfn == cmplss ? cmplssu : cmpgtru);
    }
    exprseq_init(&newargs);
    for (arg = exprseq_remhead(arglist); arg != 0; arg = exprseq_remhead(arglist)) {
        if (expr_type(arg) == EXPTYPE_PRIM_LIT) {
            long thisval = expr_litval(arg);
            if (did1) {
                if (cmpfn(thisval, curmax)) {
                    curmax = thisval;
                }
            } else {
                curmax = thisval;
                did1 = 1;
            }
            expr_node_free(ctx, arg);
        } else {
            exprseq_instail(&newargs, arg);
        }
    }
    result = 0;
    if (did1) {
        arg = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, curpos);
        expr_litval_set(arg, curmax);
        expr_has_value_set(arg, 1);
        expr_is_ctce_set(arg, 1);
        if (exprseq_length(&newargs) == 0) {
            result = arg;
        } else {
            exprseq_instail(&newargs, arg);
        }
    }
    if (result == 0) {
        result = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
        expr_func_name_set(result, fnp);
        exprseq_append(expr_func_arglist(result), &newargs);
        expr_has_value_set(result, 1);
    }

    return result;

} /* func_MINMAX */

/*
 * execfunc_SIGN
 *
 * SIGN() function.
 */
static expr_node_t *
execfunc_SIGN (expr_ctx_t ctx, compare_fn fn __attribute__((unused)),
               name_t *fnp, exprseq_t *arglist, textpos_t curpos)
{
    expr_node_t *result = exprseq_head(arglist);

    if (expr_type(result) == EXPTYPE_PRIM_LIT) {
        long val = expr_litval(result);
        result = exprseq_remhead(arglist);
        expr_litval_set(result, (val == 0 ? 0 : (val > 0 ? 1L : -1L)));
    } else {
        result = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
        expr_func_name_set(result, fnp);
        exprseq_append(expr_func_arglist(result), arglist);
        expr_has_value_set(result, 1);
    }

    return result;

} /* execfunc_SIGN */

/*
 * execfunc_ABS
 *
 * ABS() function.
 */
static expr_node_t *
execfunc_ABS (expr_ctx_t ctx, compare_fn fn __attribute__((unused)), name_t *fnp,
              exprseq_t *arglist, textpos_t curpos)
{
    expr_node_t *result = exprseq_head(arglist);
    machinedef_t *mach = expr_machinedef(ctx);

    if (expr_type(result) == EXPTYPE_PRIM_LIT) {
        long val = expr_litval(result);
        result = exprseq_remhead(arglist);
        expr_litval_set(result, getvalue(labs(val), machine_scalar_bits(mach), 0));
    } else {
        result = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
        expr_func_name_set(result, fnp);
        exprseq_append(expr_func_arglist(result), arglist);
        expr_has_value_set(result, 1);
    }

    return result;

} /* execfunc_ABS */
