/*
 *++
 * expr_control.c - Control expressions
 *
 *
 * This module parses control expressions.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "blissc/expression.h"
#include "blissc/declarations.h"
#include "blissc/symbols.h"

static expr_node_t *parse_condexp(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_case(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_select(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_incrdecr(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_wu_loop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_do_loop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_leave(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_exitloop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_return(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);

/*
 * expr_control_init
 *
 * Module initialization.
 */
void
expr_control_init (expr_ctx_t ctx)
{

    expr_dispatch_register(ctx, LEXTYPE_CTRL_IF, parse_condexp);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_CASE, parse_case);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECT, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECTA, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECTU, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECTONE, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECTONEA, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_SELECTONEU, parse_select);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_INCR, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_INCRA, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_INCRU, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_DECR, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_DECRA, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_DECRU, parse_incrdecr);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_WHILE, parse_wu_loop);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_UNTIL, parse_wu_loop);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_DO, parse_do_loop);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_LEAVE, parse_leave);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_EXITLOOP, parse_exitloop);
    expr_dispatch_register(ctx, LEXTYPE_CTRL_RETURN, parse_return);

} /* expr_control_init */

/*
 * parse_condexp
 *
 * IF test THEN consequent {ELSE alternative}
 */
static expr_node_t *
parse_condexp (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    expr_node_t *test, *cons, *alt, *exp;
    parse_ctx_t pctx = expr_parse_ctx(ctx);

    test = cons = alt = 0;
    if (!expr_parse_expr(ctx, &test)) {
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_THEN, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "THEN");
        return 0;
    }
    // Evidently, empty expressions are allowed here, although
    // the LRM doesn't indicate that that is the case. XXX
    if (!expr_parse_expr(ctx, &cons)) {
        cons = expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx));
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_ELSE, 0, 1)) {
        if (!expr_parse_expr(ctx, &alt)) {
            alt = expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx));
        }
    }

    // Optimize away constant tests, as well as conditionals
    // that have no consequent or alternative
    if (expr_type(test) == EXPTYPE_PRIM_LIT) {
        if (expr_litval(test) & 1) {
            exp = cons;
        } else {
            exp = (alt == 0 ?
                   expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx)) :
                   alt);
            expr_node_free(ctx, cons);
        }
        expr_node_free(ctx, test);
    } else if (expr_type(cons) == EXPTYPE_NOOP && (alt == 0 || expr_type(alt) == EXPTYPE_NOOP)) {
        if (alt != 0) expr_node_free(ctx, alt);
        expr_node_free(ctx, test);
        exp = cons;
    } else {
        exp = expr_node_alloc(ctx, EXPTYPE_CTRL_COND, parser_curpos(pctx));
        expr_cond_test_set(exp, test);
        expr_cond_consequent_set(exp, cons);
        expr_cond_alternative_set(exp, alt);
        expr_has_value_set(exp, (alt != 0 && expr_has_value(cons)
                                 && expr_has_value(alt)));
    }

    return exp;

} /* parse_condexp */

/*
 * parse_case
 *
 * CASE exp FROM ctce TO ctce OF
 * SET
 * [ctce { TO ctce } | INRANGE | OUTRANGE,...]: expression;
 * TES
 */
static expr_node_t *
parse_case (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{

    expr_node_t *caseidx = 0;
    expr_node_t **unique, *exp;
    int *todo;
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexeme_t *lex;
    long lo, hi, ncases, i;
    int saw_inrange, saw_outrange, status;
    long unique_actions, *cases, outrange = -1;
    int every_case_has_value = 1;

    if (!expr_parse_expr(ctx, &caseidx)) {
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "FROM");
        return 0;
    }
    if (!expr_parse_ctce(ctx, 0, &lo)) {
        expr_signal(ctx, STC__EXPCTCE);
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "TO");
        return 0;
    }
    if (!expr_parse_ctce(ctx, 0, &hi)) {
        expr_signal(ctx, STC__EXPCTCE);
        return 0;
    }
    if (lo > hi) {
        long tmp;
        expr_signal(ctx, STC__BNDINVERT);
        tmp = lo;
        lo = hi;
        hi = tmp;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "OF");
        return 0;
    }
    parser_punctclass_set(pctx, PUNCT_SEMISEP_SETTES, LEXTYPE_DELIM_SEMI);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "SET");
        return 0;
    }
    ncases = (hi - lo) + 1;
    cases = malloc(ncases*sizeof(long));
    unique = malloc((ncases+1)*sizeof(expr_node_t *)); // extra for 'outrange'
    memset(cases, -1, ncases*sizeof(long));
    memset(unique, 0, (ncases+1)*sizeof(expr_node_t *));
    todo = malloc(ncases*sizeof(int));
    saw_inrange = saw_outrange = 0;
    unique_actions = 0;
    status = 1;
    while (1) {
        int is_outrange = 0;
        memset(todo, 0, sizeof(int)*ncases);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, "[");
            parser_skip_to_delim(pctx, LEXTYPE_KWD_TES);
            status = 0;
            break;
        }
        while (1) {
            lt = parser_next(pctx, QL_NORMAL, &lex);
            if (lt == LEXTYPE_KWD_INRANGE) {
                if (saw_inrange) {
                    expr_signal(ctx, STC__MULTINRNG);
                }
                saw_inrange = 1;
                for (i = 0; i < ncases; i++) {
                    todo[i] = (cases[i] < 0);
                }
            } else if (lt == LEXTYPE_KWD_OUTRANGE) {
                if (saw_outrange) {
                    expr_signal(ctx, STC__MULTOURNG);
                }
                is_outrange = saw_outrange = 1;
            } else {
                long beginval = 0, endval = 0;
                parser_insert(pctx, lex);
                if (expr_parse_ctce(ctx, 0, &beginval)) {
                    if (saw_inrange) {
                        expr_signal(ctx, STC__NUMAFTINR);
                    }
                    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
                        if (!expr_parse_ctce(ctx, 0, &endval)) {
                            expr_signal(ctx, STC__EXPCTCE);
                        } else {
                            if (beginval > endval) {
                                long tmp = beginval;
                                expr_signal(ctx, STC__BNDINVERT);
                                beginval = endval;
                                endval = tmp;
                            }
                        }
                    } else {
                        endval = beginval;
                    }
                }
                for (i = beginval-lo; i <= endval-lo; todo[i++] = 1);
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
                break;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                expr_signal(ctx, STC__DELIMEXP, ",");
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RBRACK);
                break;
            }
        } /* per-case while */
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ":");
        }
        exp = 0;
        // Empty action is OK
        if (!expr_parse_expr(ctx, &exp)) {
            exp = expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx));
        }
        if (!expr_has_value(exp)) {
            every_case_has_value = 0;
        }
        if (is_outrange) {
            if (outrange >= 0) {
                expr_signal(ctx, STC__MULTOURNG);
            }
            outrange = unique_actions;
        }
        for (i = 0; i < ncases; i++) {
            if (todo[i]) {
                if (cases[i] >= 0) {
                    expr_signal(ctx, STC__MULNUMCAS);
                }
                cases[i] = unique_actions;
            }
        }
        unique[unique_actions++] = exp;
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
                break;
            }
        } else if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
            break;
        } else {
            expr_signal(ctx, STC__DELIMEXP, ";");
        }

    } /* outer while */

    if (status) {
        if (ncases == 0 && outrange < 0) {
            expr_signal(ctx, STC__NOCASES);
            status = 0;
        }
    }

    if (status) {
        for (i = 0; i < ncases && cases[i] >= 0; i++);
        if (i < ncases) {
            expr_signal(ctx, STC__INSUFCASE);
            status = 0;
        }
    }

    if (status) {
        exp = expr_node_alloc(ctx, EXPTYPE_CTRL_CASE, parser_curpos(pctx));
        expr_case_bounds_set(exp, lo, hi);
        expr_case_outrange_set(exp, outrange);
        expr_case_actions_set(exp, unique_actions, unique);
        expr_case_cases_set(exp, cases);
        expr_case_index_set(exp, caseidx);
        expr_has_value_set(exp, every_case_has_value);
        free(todo);
        return exp;
    }

    expr_node_free(ctx, caseidx);
    free(cases);
    while (unique_actions > 0) {
        expr_node_free(ctx, unique[--unique_actions]);
    }
    free(unique);
    free(todo);

    return 0;

} /* parse_case */

/*
 * parse_select
 *
 * SELECT{A|U}|SELECTONE{A|U} exp OF
 * SET
 * [ expr { TO expr } | OTHERWISE | ALWAYS,...]: expr;
 * TES
 */
static expr_node_t *
parse_select (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    expr_node_t *si, *exp, *lo, *hi, *always, *otherwise;
    expr_node_t *selectors, *sellast, *sel;
    exprseq_t selseq;
    int every_selector_has_value = 1;
    int is_selectone;

    si = always = otherwise = 0;
    if (!expr_parse_expr(ctx, &si)) {
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "OF");
    }
    parser_punctclass_set(pctx, PUNCT_SEMISEP_SETTES, LEXTYPE_DELIM_SEMI);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "SET");
    }
    exprseq_init(&selseq);
    is_selectone = (curlt == LEXTYPE_CTRL_SELECTONE
                    || curlt == LEXTYPE_CTRL_SELECTONEU
                    || curlt == LEXTYPE_CTRL_SELECTONEA);
    while (1) {
        int is_otherwise, is_always;
        is_otherwise = is_always = 0;

        selectors = sellast = 0;
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, "[");
            parser_skip_to_delim(pctx, LEXTYPE_KWD_TES);
            break;
        }
        while (1) {
            lo = hi = 0;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OTHERWISE, 0, 1)) {
                if (otherwise != 0) {
                    expr_signal(ctx, STC__MULOTHERW);
                } else {
                    is_otherwise = 1;
                }
            } else if (!is_selectone &&
                       parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_ALWAYS, 0, 1)) {
                if (always != 0) {
                    expr_signal(ctx, STC__MULALWAYS);
                } else {
                    is_always = 1;
                }
            } else if (expr_parse_expr(ctx, &lo)) {
                if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
                    if (!expr_parse_expr(ctx, &hi)) {
                        expr_signal(ctx, STC__EXPREXP);
                    }
                }
            } else {
                expr_signal(ctx, STC__EXPREXP);
                expr_node_free(ctx, selectors);
                selectors = 0;
                break;
            }

            // If this line has ALWAYS, any other selection criteria
            // can be discarded
            if (is_always) {
                if (lo != 0) expr_node_free(ctx, lo);
                if (hi != 0) expr_node_free(ctx, hi);
                lo = hi = 0;
                is_otherwise = 0;
            }
            if (lo != 0) {
                sel = expr_node_alloc(ctx, EXPTYPE_SELECTOR, parser_curpos(pctx));
                expr_selector_lohi_set(sel, lo, hi);
                if (selectors == 0) {
                    selectors = sellast = sel;
                } else {
                    expr_selector_next_set(sellast, sel);
                    sellast = sel;
                }
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
                break;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                expr_signal(ctx, STC__DELIMEXP, ",");
            }
        } /* inner loop */
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ":");
        }
        exp = 0;
        // Empty expression is allowed here
        if (!expr_parse_expr(ctx, &exp)) {
            exp = expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx));
        }
        if (selectors == 0 && !is_always && !is_otherwise) {
            expr_node_free(ctx, exp);
        } else {
            if (is_always) {
                always = exp;
            } else {
                if (is_otherwise) {
                    otherwise = exp;
                }
                if (selectors != 0) {
                    expr_selector_action_set(selectors, exp);
                    exprseq_instail(&selseq, selectors);
                }
            }
            if (!expr_has_value(exp)) {
                every_selector_has_value = 0;
            }
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ";");
        }
    } /* outer loop */

    if (exprseq_length(&selseq) == 0 && always == 0 && otherwise == 0) {
        expr_signal(ctx, STC__SELNOSEL);
        expr_node_free(ctx, si);
        return 0;
    }

    // Spec says that if nothing matches, the value of the
    // expression should be set to -1.  This needs to be
    // done in the code generation phase.

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_SELECT, parser_curpos(pctx));
    expr_sel_index_set(exp, si);
    expr_sel_oneonly_set(exp, is_selectone);
    expr_sel_alwaysaction_set(exp, always);
    expr_sel_otherwiseaction_set(exp, otherwise);
    switch (curlt) {
        case LEXTYPE_CTRL_SELECT:
        case LEXTYPE_CTRL_SELECTONE:
            expr_sel_cmptype_set(exp, OPER_CMP_EQL);
            break;
        case LEXTYPE_CTRL_SELECTU:
        case LEXTYPE_CTRL_SELECTONEU:
            expr_sel_cmptype_set(exp, OPER_CMP_EQLU);
            break;
        case LEXTYPE_CTRL_SELECTA:
        case LEXTYPE_CTRL_SELECTONEA:
            expr_sel_cmptype_set(exp, OPER_CMP_EQLA);
            break;
        default:
            break;
    }
    exprseq_append(expr_sel_selectors(exp), &selseq);
    expr_has_value_set(exp, every_selector_has_value);

    return exp;
}

/*
 * parse_incrdecr
 *
 * INCR{A|U}|DECR{A|U} name {FROM exp} {TO exp} {BY exp}
 */
static expr_node_t *
parse_incrdecr (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    expr_node_t *fromexp, *toexp, *byexp, *body, *exp;
    strdesc_t *indexname;
    name_t *np;
    textpos_t pos;
    scopectx_t scope;

    fromexp = toexp = byexp = 0;
    scope = parser_scope_begin(pctx);
    if (!parse_decl_name(pctx, &indexname, &pos)) {
        expr_signal(ctx, STC__NAMEEXP);
        parser_scope_end(pctx);
        return 0;
    }

    // Implicit declaration of the index name as LOCAL
    np = datasym_declare(scope, indexname, 0, pos);
    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "parse_incrdecr");
        parser_scope_end(pctx);
        return 0;
    }

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        if (!expr_parse_expr(ctx, &fromexp)) {
            expr_signal(ctx, STC__EXPREXP);
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        if (!expr_parse_expr(ctx, &toexp)) {
            expr_signal(ctx, STC__EXPREXP);
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_BY, 0, 1)) {
        if (!expr_parse_expr(ctx, &byexp)) {
            expr_signal(ctx, STC__EXPREXP);
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "DO");
    }
    body = 0;
    expr_loopdepth_incr(ctx);
    if (!expr_parse_expr(ctx, &body)) {
        expr_loopdepth_decr(ctx);
        expr_node_free(ctx, fromexp);
        expr_node_free(ctx, toexp);
        expr_node_free(ctx, byexp);
        parser_scope_end(pctx);
        return 0;
    }
    expr_loopdepth_decr(ctx);
    parser_scope_pop(pctx);
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPID, parser_curpos(pctx));
    expr_idloop_scope_set(exp, scope);
    expr_idloop_index_set(exp, np);
    expr_idloop_init_set(exp, fromexp);
    expr_idloop_term_set(exp, toexp);
    expr_idloop_step_set(exp, byexp);
    expr_idloop_body_set(exp, body);
    if (curlt == LEXTYPE_CTRL_DECR ||
        curlt == LEXTYPE_CTRL_DECRA ||
        curlt == LEXTYPE_CTRL_DECRU) {
        expr_idloop_decr_set(exp, 1);
    }
    switch (curlt) {
        case LEXTYPE_CTRL_DECR:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQ);
            break;
        case LEXTYPE_CTRL_INCR:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQ);
            break;
        case LEXTYPE_CTRL_DECRU:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQU);
            break;
        case LEXTYPE_CTRL_INCRU:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQU);
            break;
        case LEXTYPE_CTRL_DECRA:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQA);
            break;
        case LEXTYPE_CTRL_INCRA:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQA);
            break;
        default:
            break;
    }
    // XXX Really should validate that all exits have values,
    //     but not sure we can do this until a later processing stage
    expr_has_value_set(exp, 1);
    return exp;
    
} /* parse_incrdecr */

/*
 * parse_leave
 *
 * LEAVE label {WITH expression}
 *
 * Forms an exit expression for leaving the labelled block.
 */
static expr_node_t *
parse_leave (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    textpos_t pos = parser_curpos(pctx);
    expr_node_t *exp, *valexp;
    lexeme_t *lex;
    name_t *lp;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
        expr_signal(ctx, STC__EXPLABEL);
        return 0;
    }
    // The label will be pointing to our 'fake' value if it's
    // a block we are currently parsing (and thus eligible to
    // be referenced in a LEAVE).
    lp = lexeme_ctx_get(lex);
    if (lp == 0 || label_block(lp) != expr_fake_label_ptr(ctx)) {
        expr_signal(ctx, STC__INVLABEL, lexeme_text(lex));
    }
    valexp = 0;
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_WITH, 0, 1)) {
        if (!expr_parse_expr(ctx, &valexp)) {
            expr_signal(ctx, STC__LBLSCPERR, lexeme_text(lex));
        }
    }
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_EXIT, pos);
    expr_exit_label_set(exp, lp);
    expr_exit_value_set(exp, valexp);
    return exp;

} /* parse_leave */

/*
 * parse_exitloop
 *
 * EXITLOOP {expression}
 *
 * Forms an exit expression for leaving the innermost loop
 * currently being parsed.
 */
static expr_node_t *
parse_exitloop (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    expr_node_t *exp, *valexp;

    if (expr_loopdepth_get(ctx) == 0) {
        expr_signal(ctx, STC__EXITNLOOP);
    }
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_EXIT, parser_curpos(expr_parse_ctx(ctx)));
    valexp = 0;
    if (expr_parse_expr(ctx, &valexp)) {
        expr_exit_value_set(exp, valexp);
        expr_has_value_set(exp, 1);
    }
    return exp;

} /* parse_exitloop */

/*
 * parse_return
 *
 * RETURN {expression}
 *
 * Forms a return expression for returning from the current
 * routine.
 */
static expr_node_t *
parse_return (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    textpos_t pos = parser_curpos(expr_parse_ctx(ctx));
    expr_node_t *exp, *valexp;
    name_t *lp;
    routine_attr_t *attr;

    lp = expr_current_routine(ctx);

    if (!expr_parse_expr(ctx, &valexp)) {
        valexp = 0;
    }
    if (lp == 0) {
        expr_signal(ctx, STC__RETNRTN);
        if (valexp != 0) expr_node_free(ctx, valexp);
        return 0;
    }
    attr = rtnsym_attr(lp);
    if (attr == 0) {
        expr_signal(ctx, STC__INTCMPERR, "parse_return");
        valexp = 0;
    } else if (attr->flags & SYM_M_NOVALUE) {
        if (valexp != 0) {
            expr_signal(ctx, STC__RETNOVAL);
        }
    } else {
        if (valexp == 0) {
            expr_signal(ctx, STC__NORETVAL);
        }
    }
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_RET, pos);
    expr_exit_label_set(exp, lp);
    expr_exit_value_set(exp, valexp);
    expr_has_value_set(exp, 0);
    return exp;

} /* parse_return */

/*
 * parse_wu_loop
 *
 * WHILE expression DO expression
 * UNTIL expression DO expression
 *
 * Forms a LOOPWU expression to represent the while or until
 * loop, indicating that the test precedes the first iteration.
 */
static expr_node_t *
parse_wu_loop (expr_ctx_t ctx, lextype_t opener, lexeme_t *curlex)
{
    expr_node_t *body, *test, *exp;

    body = test = 0;
    if (!expr_parse_expr(ctx, &test)) {
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    if (!parser_expect(expr_parse_ctx(ctx), QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "DO");
        return 0;
    }
    expr_loopdepth_incr(ctx);
    if (!expr_parse_expr(ctx, &body)) {
        expr_loopdepth_decr(ctx);
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    expr_loopdepth_decr(ctx);

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPWU, parser_curpos(expr_parse_ctx(ctx)));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, (lexeme_type(curlex) == LEXTYPE_CTRL_WHILE
                               ? LOOP_PRETEST_WHILE : LOOP_PRETEST_UNTIL));
    expr_wuloop_body_set(exp, body);
    // XXX should evaluate the loop exits for values
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_wu_loop */

/*
 * parse_do_loop
 *
 * DO expression WHILE expression
 * DO expression UNTIL expression
 *
 * Forms a LOOPWU expression node to represent the while or
 * until loop, indicating post-test.
 */
static expr_node_t *
parse_do_loop (expr_ctx_t ctx, lextype_t opener, lexeme_t *curlex)
{
    lextype_t lt;
    lexeme_t *lex;
    expr_node_t *body, *test, *exp;

    body = test = 0;
    expr_loopdepth_incr(ctx);
    if (!expr_parse_expr(ctx, &body)) {
        expr_loopdepth_decr(ctx);
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }
    expr_loopdepth_decr(ctx);

    lt = parser_next(expr_parse_ctx(ctx), QL_NORMAL, &lex);
    if (lt != LEXTYPE_CTRL_WHILE && lt != LEXTYPE_CTRL_UNTIL) {
        expr_signal(ctx, STC__KWDEXP, "WHILE/UNTIL");
        parser_insert(expr_parse_ctx(ctx), lex);
        return 0;
    }
    lexeme_free(expr_lexmemctx(ctx), lex);

    if (!expr_parse_expr(ctx, &test)) {
        expr_signal(ctx, STC__EXPREXP);
        return 0;
    }

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPWU, parser_curpos(expr_parse_ctx(ctx)));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, (lt == LEXTYPE_CTRL_WHILE
                               ? LOOP_POSTTEST_WHILE : LOOP_POSTTEST_UNTIL));
    expr_wuloop_body_set(exp, body);
    // XXX should evaluate the loop exits for values
    expr_has_value_set(exp, 1);
    return exp;
    
} /* parse_do_loop */
