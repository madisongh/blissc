/*
 *++
 * charfuncs.c - Character-handling function package.
 *
 * This module implements the CH$ executable functions.  It hooks i
 * to the executable function handling system in the execfuncs module.
 * A subset of the CH$ functions are implemented (partially, at least) in
 * the front end, as the LRM specifies certain behavior when they are
 * applied to compile-time constant expressions.  Others are "passed
 * through" to the back-end code generator.  See the table in the
 * initialization function at the bottom of this module for info on
 * which is which.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license.  See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include "blissc/execfuncs.h"
#include "blissc/expression.h"
#include "blissc/symbols.h"
#include "blissc/parser.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"

/*
 * charfunc_ALLOCATION
 *
 * CH$ALLOCATION(n [,cs])
 *
 * Returns the allocation, in fullwords to accommodate a
 * character sequence of length 'n'.  If the character size
 * 'cs' is not specified, the machine's default character size
 * is used in the calculation.  If everything here is CTCE,
 * a CTCE is returned.
 */
static expr_node_t *
charfunc_ALLOCATION (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
                exprseq_t *arglist, textpos_t curpos)
{
    machinedef_t *mach = expr_machinedef(ctx);
    expr_node_t *e;
    unsigned int i = exprseq_length(arglist);
    unsigned int cs = machine_charsize(mach, 0);

    if (i > 2) {
        expr_signal(ctx, STC__EXCFUNARGS, name_string(fnp));
    }
    if (i > 1) {
        e = exprseq_head(arglist)->tq_next;
        if (expr_type(e) != EXPTYPE_PRIM_LIT) {
            expr_signal(ctx, STC__CSNOTCONST);
        } else {
            cs = (unsigned int) expr_litval(e);
            for (i = 0; i < machine_cs_count(mach) && cs != machine_charsize(mach, i); i++);
            if (i >= machine_cs_count(mach)) {
                expr_signal(ctx, STC__INVCSIZE, cs);
                cs = machine_charsize(mach, 0);
            }
        }
    }
    e = exprseq_head(arglist);
    if (expr_type(e) == EXPTYPE_PRIM_LIT) {
        long n = expr_litval(e);
        e = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, curpos);
        expr_litval_set(e, (n * cs + machine_scalar_bits(mach)-1)/machine_scalar_bits(mach));
        expr_has_value_set(e, 1);
        expr_is_ctce_set(e, 1);
        return e;
    }
    e = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
    expr_func_name_set(e, fnp);
    exprseq_append(expr_func_arglist(e), arglist);
    expr_has_value_set(e, 1);
    return e;

} /* charfunc_ALLOCATION */

/*
 * charfunc_SIZE
 *
 * CH$SIZE([cs])
 *
 * Character size function.  If no parameter is
 * specified, and the machine supports only one
 * character size, this is CTCE.
 */
static expr_node_t *
charfunc_SIZE (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
          exprseq_t *arglist, textpos_t curpos)
{
    machinedef_t *mach = expr_machinedef(ctx);
    expr_node_t *e;
    unsigned int i = exprseq_length(arglist);
    unsigned int cs = machine_charsize(mach, 0);

    if (i > 1) {
        expr_signal(ctx, STC__EXCFUNARGS, name_string(fnp));
    }
    if (i == 0 || machine_cs_count(mach) == 1) {
        e = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, curpos);
        expr_litval_set(e, cs);
        expr_has_value_set(e, 1);
        expr_is_ctce_set(e, 1);
        return e;
    }
    e = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
    expr_func_name_set(e, fnp);
    exprseq_append(expr_func_arglist(e), arglist);
    expr_has_value_set(e, 1);
    return e;

} /* charfunc_SIZE */

/*
 * charfunc_PTR
 *
 * CH$PTR(seg [,offset] [,cs])
 *
 * Returns a pointer to the character sequence at 'offset'
 * characters past 'seg'.  'cs' represents the character
 * size.
 */
static expr_node_t *
charfunc_PTR (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
         exprseq_t *arglist, textpos_t curpos)
{
    machinedef_t *mach = expr_machinedef(ctx);
    expr_node_t *e;
    unsigned int i = exprseq_length(arglist);
    int is_ltce = 0;

    if (i > 3) {
        expr_signal(ctx, STC__EXCFUNARGS, name_string(fnp));
    }
    if (i > 2) {
        e = exprseq_head(arglist)->tq_next->tq_next;
        if (expr_type(e) != EXPTYPE_PRIM_LIT) {
            expr_signal(ctx, STC__CSNOTCONST);
        } else {
            unsigned int cs = (unsigned int) expr_litval(e);
            for (i = 0; i < machine_cs_count(mach) && cs != machine_charsize(mach, i); i++);
            if (i >= machine_cs_count(mach)) {
                expr_signal(ctx, STC__INVCSIZE, cs);
            }
        }
    }
    if (i > 1) {
        e = exprseq_head(arglist)->tq_next;
        is_ltce = (expr_is_ctce(e) && expr_is_ltce(exprseq_head(arglist)));
    } else {
        is_ltce = expr_is_ltce(exprseq_head(arglist));
    }
    e = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
    expr_func_name_set(e, fnp);
    exprseq_append(expr_func_arglist(e), arglist);
    expr_has_value_set(e, 1);
    expr_is_ltce_set(e, is_ltce);

    return e;

} /* charfunc_PTR */

/*
 * charfunc_DIFF
 *
 * CH$DIFF(ptr1, ptr2)
 *
 * Returns the difference between 'ptr1' and 'ptr2'.
 * XXX This currently assumes that a simple subtraction
 * is adequate for this operation.
 */
static expr_node_t *
charfunc_DIFF (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
          exprseq_t *arglist, textpos_t curpos)
{
    expr_node_t *e, *arg;

    e = expr_node_alloc(ctx, EXPTYPE_OPERATOR, curpos);
    expr_op_type_set(e, OPER_SUBTRACT);
    arg = exprseq_remhead(arglist);
    expr_op_lhs_set(e, arg);
    arg = exprseq_remhead(arglist);
    expr_op_rhs_set(e, arg);
    expr_has_value_set(e, 1);

    return e;

} /* charfunc_DIFF */

/*
 * charfunc_passthru
 *
 * Pass-through function - simply creates an expression node
 * so the back-end code generator can process the function.
 */
static expr_node_t *
charfunc_passthru (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
              exprseq_t *arglist, textpos_t curpos)
{
    expr_node_t *e;
    funcdef_t *this_func = name_extraspace(fnp);

    e = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
    expr_func_name_set(e, fnp);
    exprseq_append(expr_func_arglist(e), arglist);
    expr_has_value_set(e, (this_func->flags & FUNC_M_NOVALUE) == 0);
    return e;

} /* charfunc_passthru */

/*
 * tt_items
 *
 * Watered-down version of plit_items() that handles the
 * subset of the syntax needed for CH$TRANSTABLE.
 *
 */
static initval_t *
tt_items (expr_ctx_t ctx) {

    symctx_t symctx = expr_symctx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    initval_t *ivlist, *iv;
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    ivlist = 0;
    while (1) {
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REP, 0, 1)) {
            long val;
            unsigned int repcount;
            if (!expr_parse_ctce(ctx, 0, &val)) {
                expr_signal(ctx, STC__EXPCTCE);
                repcount = 1;
            } else {
                repcount = (unsigned int) val;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
                expr_signal(ctx, STC__KWDEXP, "OF");
            }
            parser_punctclass_set(pctx, PUNCT_COMMASEP_PARENS, LEXTYPE_DELIM_COMMA);
            iv = tt_items(ctx);
            if (iv != 0) {
                ivlist = initval_ivlist_add(symctx, ivlist, repcount, iv);
            }
        } else {
            expr_node_t *exp;
            lex = 0;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
                ivlist = initval_string_add(symctx, ivlist, 1, lexeme_text(lex));
                lexeme_free(lctx, lex);
            } else {
                long val;
                if (!expr_parse_ctce(ctx, 0, &val)) {
                    expr_signal(ctx, STC__EXPCTCE);
                    val = 0;
                }
                exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
                expr_litval_set(exp, val);
                ivlist = expr_initval_add(ctx, ivlist, exp, 1);
            }
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
        }
    }
    return ivlist;

} /* tt_items */

/*
 * charfunc_TRANSTABLE
 *
 * Parses a character translation table, resulting in a
 * counted PLIT.  Note that the arguments are NOT pre-parsed
 * by the executable-function binding routine.
 */
static expr_node_t *
charfunc_TRANSTABLE (expr_ctx_t ctx, compare_fn fn, name_t *fnp,
                exprseq_t *arglist, textpos_t curpos)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    symctx_t symctx = expr_symctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    name_t *psname, *np;
    char namebuf[NAME_SIZE];
    strdesc_t plitname;
    data_attr_t attr;
    initval_t *ivlist;
    unsigned long size;
    unsigned int padding;
    expr_node_t *e;

    psname = scope_sclass_psectname(parser_scope_get(pctx), SCLASS_PLIT);
    ivlist = tt_items(ctx);
    if (ivlist == 0) {
        return 0;
    }
    size = initval_size(symctx, ivlist);
    // XXX magic constant here
    padding = (size >= 256 ? 0 : 256 - (unsigned int) size);
    if (padding != 0) {
        ivlist = initval_scalar_add(symctx, ivlist, padding, 0, 1, 0);
    }
    np = 0;
    strdesc_init(&plitname, namebuf, 0);
    plitname.len = tempname_get(expr_namectx(ctx), namebuf, sizeof(namebuf));
    memset(&attr, 0, sizeof(attr));
    attr.owner = psname;
    attr.flags = 0;
    attr.dclass = DCLASS_STATIC;
    attr.ivlist = ivlist;
    attr.units = (unsigned int) initval_size(symctx, ivlist);
    attr.sc = SYMSCOPE_LOCAL;
    np = datasym_declare(parser_scope_get(pctx), &plitname, &attr, curpos);
    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "charfunc_TRANSTABLE");
    }

    e = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, curpos);
    expr_seg_name_set(e, np);
    expr_seg_width_set(e, machine_unit_bits(mach));
    expr_has_value_set(e, 1);
    expr_is_ltce_set(e, 1);

    return e;

} /* charfunc_TRANSTABLE */

static funcdef_t charfunc_funcs[] = {
    FUNCDEF("CH$ALLOCATION",    charfunc_ALLOCATION, 0, 1, FUNC_M_VARARGS),
    FUNCDEF("CH$SIZE",          charfunc_SIZE,       0, 0, FUNC_M_VARARGS),
    FUNCDEF("CH$PTR",           charfunc_PTR,        0, 1, FUNC_M_VARARGS),
    FUNCDEF("CH$PLUS",          charfunc_passthru,   0, 2, 0),
    FUNCDEF("CH$DIFF",          charfunc_DIFF,       0, 2, 0),
    FUNCDEF("CH$RCHAR",         charfunc_passthru,   0, 1, 0),
    FUNCDEF("CH$A_RCHAR",       charfunc_passthru,   0, 1, 0),
    FUNCDEF("CH$RCHAR_A",       charfunc_passthru,   0, 1, 0),
    FUNCDEF("CH$WCHAR",         charfunc_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$A_WCHAR",       charfunc_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$WCHAR_A",       charfunc_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$MOVE",          charfunc_passthru,   0, 3, 0),
    FUNCDEF("CH$FILL",          charfunc_passthru,   0, 3, 0),
    FUNCDEF("CH$COPY",          charfunc_passthru,   0, 5, FUNC_M_VARARGS),
    FUNCDEF("CH$EQL",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$NEQ",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$LSS",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$LEQ",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$GTR",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$GEQ",           charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$COMPARE",       charfunc_passthru,   0, 5, 0),
    FUNCDEF("CH$FIND_CH",       charfunc_passthru,   0, 3, 0),
    FUNCDEF("CH$FIND_NOT_CH",   charfunc_passthru,   0, 3, 0),
    FUNCDEF("CH$FIND_SUB",      charfunc_passthru,   0, 4, 0),
    FUNCDEF("CH$FAIL",          charfunc_passthru,   0, 1, 0),
    FUNCDEF("CH$TRANSTABLE",    charfunc_TRANSTABLE, 0, 0, FUNC_M_NOPARSE),
    FUNCDEF("CH$TRANSLATE",     charfunc_passthru,   0, 6, 0)
};

/*
 * charfuncs_init
 *
 * Initializes the execfuncs hooks for the CH$ functions.
 */
void
charfuncs_init (expr_ctx_t ctx, scopectx_t scope)
{
    unsigned int i;
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_FUNCTION;
    for (i = 0; i < sizeof(charfunc_funcs)/sizeof(charfunc_funcs[0]); i++) {
        ndef.name = charfunc_funcs[i].name;
        ndef.namelen = charfunc_funcs[i].namelen;
        name_declare(scope, &ndef, 0, &charfunc_funcs[i], sizeof(charfunc_funcs[i]), 0);
    }

} /* charfuncs_init */
