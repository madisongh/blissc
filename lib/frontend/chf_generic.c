/*
 *++
 *	File:			chf_generic.c
 *
 *	Abstract:		Character-handling function package.
 *
 *  Module description:
 *		This module implements the CHF$ executable functions.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		19-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include "blissc/execfuncs.h"
#include "blissc/expression.h"
#include "blissc/symbols.h"
#include "blissc/parser.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"

static expr_node_t *
chf_ALLOCATION (expr_ctx_t ctx, void *fctx, name_t *fnp,
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

} /* chf_ALLOCATION */

static expr_node_t *
chf_SIZE (expr_ctx_t ctx, void *fctx, name_t *fnp,
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

} /* chf_SIZE */

static expr_node_t *
chf_PTR (expr_ctx_t ctx, void *fctx, name_t *fnp,
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

} /* chf_PTR */

static expr_node_t *
chf_DIFF (expr_ctx_t ctx, void *fctx, name_t *fnp,
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

} /* chf_DIFF */

static expr_node_t *
chf_passthru (expr_ctx_t ctx, void *fctx, name_t *fnp,
              exprseq_t *arglist, textpos_t curpos)
{
    expr_node_t *e;
    funcdef_t *this_func = name_extraspace(fnp);

    e = expr_node_alloc(ctx, EXPTYPE_EXECFUN, curpos);
    expr_func_name_set(e, fnp);
    exprseq_append(expr_func_arglist(e), arglist);
    expr_has_value_set(e, (this_func->flags & FUNC_M_NOVALUE) == 0);
    return e;

} /* chf_passthru */

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
 * chf_TRANSTABLE
 *
 * Parses a character translation table, resulting in a
 * counted PLIT.  Note that the arguments are NOT pre-parsed
 * by the executable-function binding routine.
 */
static expr_node_t *
chf_TRANSTABLE (expr_ctx_t ctx, void *fctx, name_t *fnp,
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
    padding = machine_scalar_units(mach) - (size == 0 ? 0
                                            : (size % machine_scalar_units(mach)));
    // Must pad out to integral number of fullwords
    if (padding != 0) {
        ivlist = initval_scalar_add(symctx, ivlist, padding, 0, 1, 0);
    }
    size = initval_size(symctx, ivlist) / machine_scalar_units(mach);
    ivlist = initval_scalar_prepend(symctx, ivlist, 1, size,
                                    machine_scalar_units(mach), 0);

    np = 0;
    strdesc_init(&plitname, namebuf, 0);
    plitname.len = tempname_get(expr_namectx(ctx), namebuf, sizeof(namebuf));
    memset(&attr, 0, sizeof(attr));
    attr.owner = psname;
    attr.flags = 0;
    attr.dclass = DCLASS_STATIC;
    attr.ivlist = ivlist;
    attr.units = (unsigned int) (initval_size(symctx, ivlist) / machine_scalar_units(mach));
    attr.sc = SYMSCOPE_LOCAL;
    np = datasym_declare(parser_scope_get(pctx), &plitname, &attr, curpos);
    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "chf_TRANSTABLE");
    }

    e = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, curpos);
    expr_seg_name_set(e, np);
    expr_seg_width_set(e, machine_unit_bits(mach));
    expr_has_value_set(e, 1);
    expr_is_ltce_set(e, 1);

    return e;
    
} /* chf_TRANSTABLE */

static funcdef_t chf_funcs[] = {
    FUNCDEF("CH$ALLOCATION",    chf_ALLOCATION, 0, 1, FUNC_M_VARARGS),
    FUNCDEF("CH$SIZE",          chf_SIZE,       0, 0, FUNC_M_VARARGS),
    FUNCDEF("CH$PTR",           chf_PTR,        0, 1, FUNC_M_VARARGS),
    FUNCDEF("CH$PLUS",          chf_passthru,   0, 2, 0),
    FUNCDEF("CH$DIFF",          chf_DIFF,       0, 2, 0),
    FUNCDEF("CH$RCHAR",         chf_passthru,   0, 1, 0),
    FUNCDEF("CH$A_RCHAR",       chf_passthru,   0, 1, 0),
    FUNCDEF("CH$RCHAR_A",       chf_passthru,   0, 1, 0),
    FUNCDEF("CH$WCHAR",         chf_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$A_WCHAR",       chf_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$WCHAR_A",       chf_passthru,   0, 2, FUNC_M_NOVALUE),
    FUNCDEF("CH$MOVE",          chf_passthru,   0, 3, 0),
    FUNCDEF("CH$FILL",          chf_passthru,   0, 3, 0),
    FUNCDEF("CH$COPY",          chf_passthru,   0, 5, FUNC_M_VARARGS),
    FUNCDEF("CH$EQL",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$NEQ",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$LSS",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$LEQ",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$GTR",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$GEQ",           chf_passthru,   0, 5, 0),
    FUNCDEF("CH$COMPARE",       chf_passthru,   0, 5, 0),
    FUNCDEF("CH$FIND_CH",       chf_passthru,   0, 3, 0),
    FUNCDEF("CH$FIND_NOT_CH",   chf_passthru,   0, 3, 0),
    FUNCDEF("CH$FIND_SUB",      chf_passthru,   0, 4, 0),
    FUNCDEF("CH$FAIL",          chf_passthru,   0, 1, 0),
    FUNCDEF("CH$TRANSTABLE",    chf_TRANSTABLE, 0, 0, FUNC_M_NOPARSE),
    FUNCDEF("CH$TRANSLATE",     chf_passthru,   0, 6, 0)
};

void
chf_init (expr_ctx_t ctx, scopectx_t scope)
{
    int i;
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_FUNCTION;
    for (i = 0; i < sizeof(chf_funcs)/sizeof(chf_funcs[0]); i++) {
        ndef.name = chf_funcs[i].name;
        ndef.namelen = chf_funcs[i].namelen;
        name_declare(scope, &ndef, 0, &chf_funcs[i], sizeof(chf_funcs[i]), 0);
    }

} /* chf_init */
