//
//  macros.c
//  blissc
//
//  Created by Matthew Madison on 11/6/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "declarations.h"
#include "parser.h"
#include "expression.h"
#include "lexer.h"
#include "nametable.h"
#include "lexeme.h"
#include "strings.h"
#include "macros.h"

#define PARAM_ALLOCOUNT 64


typedef enum {
    MACRO_UNK, MACRO_SIMPLE, MACRO_COND, MACRO_ITER, MACRO_KWD
} macrotype_t;

struct macrodecl_s {
    lexseq_t body;
    macrotype_t type;
    namereflist_t plist, ilist;
    scopectx_t ptable;
};

static namedef_t macro_names[] = {
    NAMEDEF("KEYWORDMACRO", LEXTYPE_DCL_KEYWORDMACRO, NAME_M_RESERVED),
    NAMEDEF("MACRO", LEXTYPE_DCL_MACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITMACRO", LEXTYPE_LXF_EXITMACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITITERATION", LEXTYPE_LXF_EXITITER, NAME_M_RESERVED),
    NAMEDEF("%ERRORMACRO", LEXTYPE_LXF_ERRORMACRO, NAME_M_RESERVED),
    NAMEDEF("%REMAINING", LEXTYPE_LXF_REMAINING, NAME_M_RESERVED),
    NAMEDEF("%COUNT", LEXTYPE_LXF_COUNT, NAME_M_RESERVED),
    NAMEDEF("%LENGTH", LEXTYPE_LXF_LENGTH, NAME_M_RESERVED)
};

static lextype_t openers[] = { LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LBRACK,
                               LEXTYPE_DELIM_LANGLE };
static lextype_t closers[] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RBRACK,
                               LEXTYPE_DELIM_RANGLE };

static int macro_expand(parse_ctx_t pctx, name_t *macroname,
                        lexseq_t *result);

/*
 * macro_bind
 */
static int
macro_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
            lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    parse_ctx_t pctx = ctx;
    name_t *np = lexeme_ctx_get(lex);

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lctx, lex);
        return 1;
    }

    switch (qm) {
        case QM_QUOTE:
            lex->type = LEXTYPE_UNBOUND;
            return 0;
        case QM_EXPAND:
            lexeme_free(lctx, lex);
            return macro_expand(pctx, np, result);
        case QM_UNQUOTE:
            lex->type = LEXTYPE_NAME_MACRO;
            return 0;
        default:
            break;
    }

    // no quote modifier

    if (ql == QL_MACRO) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexeme_free(lctx, lex);
    return macro_expand(pctx, np, result);

} /* macro_bind */

/*
 * macparam_bind
 */
static int
macparam_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    name_t *np = lexeme_ctx_get(lex);
    lexseq_t tmpseq;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lctx, lex);
        return 1;
    }
    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexseq_init(&tmpseq);
    lexseq_copy(lctx, &tmpseq, name_extraspace(np));
    lexseq_append(result, &tmpseq);
    lexeme_free(lctx, lex);

    return 1;

} /* macparam_bind */

lexseq_t *
macparam_lexseq (name_t *np)
{
    return name_extraspace(np);
}

static int
macparam_initdata (void *vctx, name_t *np, void *p)
{
    lexseq_t *seq = p;
    lexseq_init(seq);
    return 1;
}
/*
 * macparam_freedata
 *
 * Frees up a macro parameter (in the name structure).
 */
static void
macparam_freedata (void *vctx, name_t *np, void *p)
{
    lexctx_t lctx = expr_lexmemctx(vctx);
    lexseq_t *seq = p;
    lexseq_free(lctx, seq);

} /* macparam_freedata */

/*
 * macparam_copydata
 *
 * Copies a macro parameter.
 */
static int
macparam_copydata (void *vctx, name_t *dst, void *dp, name_t *src, void *sp)
{
    lexctx_t lctx = expr_lexmemctx(vctx);
    lexseq_copy(lctx, dp, sp);
    return 1;
}

static int
macro_initdata (void *vctx, name_t *np, void *p)
{
    struct macrodecl_s *macro = p;

    namereflist_init(&macro->plist);
    namereflist_init(&macro->ilist);
    macro->ptable = 0;
    lexseq_init(&macro->body);
    return 1;
}

/*
 * macro_freedata
 *
 * Releases a macro's storage.  Called by name_free().
 */
static void
macro_freedata (void *vctx, name_t *np, void *p)
{
    struct macrodecl_s *macro = p;
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);

    namereflist_free(namectx, &macro->plist);
    namereflist_free(namectx, &macro->ilist);
    lexseq_free(lctx, &macro->body);
    scope_end(macro->ptable);

} /* macro_freedata */

/*
 * macro_copydata
 *
 * Copies a macro's storage.  Called by name_copy().
 */
static int
macro_copydata (void *vctx, name_t *dst, void *dp, name_t *src, void *sp)
{
    struct macrodecl_s *srcm = sp;
    struct macrodecl_s *dstm = dp;
    nameref_t *ref;
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);

    lexseq_init(&dstm->body);
    lexseq_copy(lctx, &dstm->body, &srcm->body);
    dstm->ptable = scope_copy(srcm->ptable, 0);
    namereflist_init(&dstm->plist);
    namereflist_init(&dstm->ilist);
    dstm->type = srcm->type;
    for (ref = namereflist_head(&srcm->plist); ref != 0; ref = ref->tq_next) {
        name_t *np;
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *namestr = name_string(ref->np);
            np = name_search(dstm->ptable, namestr->ptr,
                             namestr->len, 0);
        }
        namereflist_instail(&dstm->plist, nameref_alloc(namectx, np));
    }
    for (ref = namereflist_head(&srcm->ilist); ref != 0; ref = ref->tq_next) {
        name_t *np;
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *namestr = name_string(ref->np);
            np = name_search(dstm->ptable, namestr->ptr,
                             namestr->len, 0);
        }
        namereflist_instail(&dstm->ilist, nameref_alloc(namectx, np));
    }

    return 1;

} /* macro_copydata */


/*
 * macros_init
 *
 * Initialization routine.
 */
void
macros_init (scopectx_t kwdscope, expr_ctx_t ctx)
{
    namectx_t namectx = scope_namectx(kwdscope);
    lexctx_t lctx = expr_lexmemctx(ctx);
    nametype_vectors_t vec;
    int i;

    for (i = 0; i < sizeof(macro_names)/sizeof(macro_names[0]); i++) {
        name_declare(kwdscope, &macro_names[i], 0, 0, 0, 0);
    }

    lextype_register(lctx, LEXTYPE_NAME_MACRO, macro_bind);
    lextype_register(lctx, LEXTYPE_NAME_MAC_PARAM, macparam_bind);
    memset(&vec, 0, sizeof(vec));
    vec.typesize = sizeof(struct macrodecl_s);
    vec.typeinit = macro_initdata;
    vec.typefree = macro_freedata;
    vec.typecopy = macro_copydata;
    nametype_dataop_register(namectx, LEXTYPE_NAME_MACRO, &vec, ctx);
    memset(&vec, 0, sizeof(vec));
    vec.typesize = sizeof(lexseq_t);
    vec.typeinit = macparam_initdata;
    vec.typefree = macparam_freedata;
    vec.typecopy = macparam_copydata;
    nametype_dataop_register(namectx, LEXTYPE_NAME_MAC_PARAM, &vec, ctx);

} /* macros_init */

name_t *
macparam_special (scopectx_t scope, strdesc_t *pname, lexseq_t *seqval)
{
    namedef_t ndef;

    ndef.lt = LEXTYPE_NAME_MAC_PARAM;
    ndef.name = pname->ptr;
    ndef.namelen = pname->len;

    return name_declare_nocheck(scope, &ndef, 0, seqval, sizeof(lexseq_t), 0);
}

name_t *
macparam_lookup (lexctx_t lctx, scopectx_t scope, strdesc_t *pname, lexseq_t *value)
{
    name_t *np;
    lexseq_t *seq;

    np = name_search_typed(scope, pname->ptr, pname->len,
                           LEXTYPE_NAME_MAC_PARAM, &seq);
    if (np != 0 && value != 0) {
        lexseq_copy(lctx, value, seq);
    }

    return np;
}

/*
 * macro_paramlist
 *
 * Parse a macro or structure parameter list, for declarations.
 *
 * For keyword macros and structure declarations,
 * handles the default value setting.
 *
 * For non-keyword macros, handles simple, conditional, and
 * iterative macro parameter lists.
 *
 * Returns: -1 on error
 *          index of closing delimiter in delims[] array on success
 */
int
macro_paramlist (parse_ctx_t pctx, scopectx_t curscope,
                 int assign_allowed, int for_macro,
                 lextype_t delims[], int ndelims,
                 scopectx_t *ptable, namereflist_t *plist)
{
    namectx_t namectx = scope_namectx(parser_scope_get(pctx));
    lexctx_t lctx = parser_lexmemctx(pctx);
    lexeme_t *lex;
    lextype_t lt;
    strdesc_t *ltext;
    name_t *mnp;
    scopectx_t pscope;
    lexseq_t nullseq;
    namedef_t ndef;
    int i;
    lextype_t terms[16];

    // Need a delimiter array that adds ',' to the
    // caller's list
    if (ndelims >= 16) return -1;
    memcpy(terms, delims, sizeof(lextype_t)*ndelims);
    terms[ndelims] = LEXTYPE_DELIM_COMMA;

    if (*ptable == 0) {
        pscope = scope_begin(scope_namectx(curscope), curscope);
    } else {
        pscope = *ptable;
    }

    lexseq_init(&nullseq);
    parser_scope_begin(pctx);
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MAC_PARAM;

    while (1) {
        lexseq_t *pseq;
        lt = parser_next(pctx, QL_NAME, &lex);
        if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
            /* XXX error condition */
            break;
        }
        ltext = lexeme_text(lex);
        ndef.name = ltext->ptr;
        ndef.namelen = ltext->len;
        mnp = name_declare(pscope, &ndef,
                           lexeme_textpos_get(lex), 0, 0, &pseq);
        if (mnp == 0) {
            /* XXX error condition */
            break;
        }
        namereflist_instail(plist, nameref_alloc(namectx, mnp));
        lexeme_free(lctx, lex);
        lt = parser_next(pctx, QL_NAME, &lex);
        if (assign_allowed && lt == LEXTYPE_OP_ASSIGN) {
            int status;
            lexeme_free(lctx, lex);
            if (for_macro) {
                status = parse_lexeme_seq(pctx, 0, QL_MACRO,
                                          terms, ndelims+1, pseq, &lt);
            } else {
                status = expr_parse_ctce(parser_get_expctx(pctx), &lex);
                if (status) lexseq_instail(pseq, lex);
                lt = parser_next(pctx, QL_NORMAL, 0);
            }
            if (!status) {
                /* XXX error condition */
                break;
            }
        } else {
            lexeme_free(lctx, lex);
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            break;
        }
    }
    parser_scope_end(pctx);

    for (i = 0; i < ndelims && lt != delims[i]; i++);
    if (i >= ndelims) {
        scope_end(pscope);
        parser_skip_to_delim(pctx, delims[0]);
        namereflist_free(namectx, plist);
        i = -1;
    }

    return i;

} /* macro_paramlist */

/*
 * declare_macro
 *
 * Common logic for macro declarations.
 *
 * MACRO macro-name { (param,...) } { [param,...] } = {stuff} % {,...}
 * KEYWORDMACRO macro-name { (param{=defval},...) } = {stuff} % {,...}
 */
int
declare_macro (parse_ctx_t pctx, scopectx_t scope, lextype_t curlt)
{
    int skip_to_end = 0;
    lexeme_t *lex;
    lextype_t lt;
    strdesc_t *ltext;
    name_t *np;
    int is_kwdmacro = (curlt == LEXTYPE_DCL_KEYWORDMACRO);
    struct macrodecl_s *macro;
    textpos_t pos;
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MACRO;
    ndef.flags = NAME_M_DECLARED;
    while (1) {
        if (!parse_decl_name(pctx, scope, &ltext, &pos)) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        lt = parser_next(pctx, QL_NAME, 0);
        if (lt != LEXTYPE_DELIM_LPAR &&
            lt != LEXTYPE_OP_ASSIGN &&
            (is_kwdmacro || lt == LEXTYPE_DELIM_LBRACK)) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        ndef.name = ltext->ptr;
        ndef.namelen = ltext->len;
        np = name_declare(scope, &ndef, pos, 0, 0, &macro);
        macro->type = (is_kwdmacro ? MACRO_KWD : MACRO_UNK);
        if (lt == LEXTYPE_DELIM_LPAR) {
            if (macro_paramlist(pctx, scope, is_kwdmacro, 1,
                                closers, 1, &macro->ptable,
                                &macro->plist) < 0) {
                skip_to_end = 1;
            }
            if (namereflist_length(&macro->plist) == 0) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            if (!is_kwdmacro) {
                macro->type = MACRO_SIMPLE;
            }
        }
        if (lt == LEXTYPE_DELIM_LBRACK) {
            if (is_kwdmacro) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            if (macro_paramlist(pctx, scope, 0, 1, &closers[1], 1,
                                &macro->ptable, &macro->ilist) < 0) {
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            macro->type = (namereflist_length(&macro->ilist) == 0 ?
                           MACRO_COND : MACRO_ITER);
        }

        if (lt == LEXTYPE_OP_ASSIGN) {

            if (macro->type == MACRO_UNK) {
                macro->type = MACRO_SIMPLE;
            }

            parser_incr_erroneof(pctx);
            parser_scope_begin(pctx);
            for (lt = parser_next(pctx, QL_MACRO, &lex);
                 lt != LEXTYPE_LXF_DELIM_PERCENT;
                 lt = parser_next(pctx, QL_MACRO, &lex)) {
                if (lt == LEXTYPE_END || lt == LEXTYPE_NONE) {
                    /* XXX error condition */
                    skip_to_end = 1;
                    break;
                }
                lexseq_instail(&macro->body, lex);
            }
            parser_scope_end(pctx);
            parser_decr_erroneof(pctx);
        }

        if (skip_to_end) {
            name_free(np);
            string_free(ltext);
            break;
        }
        if (macro->ptable != 0) {
            scope_setparent(macro->ptable, 0);
        }
        string_free(ltext);

        lt = parser_next(pctx, QL_NAME, 0);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            break;
        }

    } /* while 1 */

    return 1;

} /* define_macro */

/*
 * prepare_body
 *
 * Builds a lexeme sequence with the expansion of the body of
 * a macro.
 */
static int
prepare_body (parse_ctx_t pctx, scopectx_t expscope, struct macrodecl_s *macro,
              unsigned int curdepth, unsigned int actcount, lexseq_t *remaining,
              lexseq_t *result, int *errorout)
{
    lexctx_t lctx = parser_lexmemctx(pctx);
    lexeme_t *lex, *bodynext;
    name_t *np;
    int do_exitmacro, do_exititer, do_errormacro, do_recursion;

    do_recursion = do_exitmacro = do_exititer = do_errormacro = 0;

    if (macro->type == MACRO_ITER) {
        lex = parser_punct_grouper(pctx, 0);
        if (lex != 0) {
            lexseq_instail(result, lex);
        }
    }

    while (1) {

        // Associate the iterative-formals with any actuals
        if (macro->type == MACRO_ITER) {
            nameref_t *iformal;
            lextype_t lt, terms[1] = { LEXTYPE_DELIM_COMMA };

            for (iformal = namereflist_head(&macro->ilist); iformal != 0;
                 iformal = iformal->tq_next) {
                strdesc_t *namestr = name_string(iformal->np);
                np = macparam_special(expscope, namestr, 0);
                parse_lexeme_seq(pctx, remaining, QL_MACRO, terms, 1,
                                 macparam_lexseq(np), &lt);
            }
        }

        // Copy the body text, checking for recursion.

        for (lex = lexseq_head(&macro->body); lex != 0; lex = bodynext) {
            lextype_t lt;
            bodynext = lexeme_next(lex);
            lex = lexeme_copy(lctx, lex);
            lt = lexeme_boundtype(lex);
            if (lt == LEXTYPE_NAME) {
                lextype_t nlt;
                np = name_search(expscope, lex->text.ptr, lex->text.len, &nlt);
                if (np != 0) {
                    struct macrodecl_s *mp = name_extraspace(np);
                    if (nlt == LEXTYPE_NAME_MACRO && mp == macro) {
                        if (macro->type != MACRO_COND) {
                            /* XXX error condition */
                            lexseq_free(lctx, result);
                            return 0;
                        }
                        do_recursion = 1;
                        lexeme_free(lctx, lex);
                        lex = 0;
                    } else if (name_scope(np) == expscope) {
                        lexseq_t valcopy;
                        lexeme_free(lctx, lex);
                        lexseq_init(&valcopy);
                        lexseq_copy(lctx, &valcopy, name_extraspace(np));
                        lexseq_append(result, &valcopy);
                        lex = 0;
                    } else {
                        lt = nlt;
                    }
                } /* if np != 0 */
            } /* if (lt == LEXTYPE_NAME) */

            // Now handle the macro-specific lexical functions

            switch (lt) {
                default:
                    break;
                case LEXTYPE_LXF_EXITMACRO:
                    do_exitmacro = 1;
                    lexeme_free(lctx, lex);
                    lex = 0;
                    break;
                case LEXTYPE_LXF_EXITITER:
                    do_exititer = 1;
                    lexeme_free(lctx, lex);
                    lex = 0;
                    break;
                case LEXTYPE_LXF_ERRORMACRO:
                    lexeme_free(lctx, lex);
                    lex = 0;
                    do_errormacro = 1;
                    break;
                case LEXTYPE_LXF_REMAINING: {
                    lexseq_t rcopy;
                    lexseq_init(&rcopy);
                    lexseq_copy(lctx, &rcopy, remaining);
                    lexseq_append(result, &rcopy);
                    lexeme_free(lctx, lex);
                    lex = 0;
                    break;
                }
                case LEXTYPE_LXF_COUNT:
                case LEXTYPE_LXF_LENGTH: {
                    strdesc_t * str;
                    str = string_printf(0, "%u",
                                        (lt == LEXTYPE_LXF_COUNT
                                         ? curdepth : actcount));
                    lexeme_free(lctx, lex);
                    lex = parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str);
                    string_free(str);
                    break;
                }
            } /* switch (lt) */

            if (do_exitmacro || do_exititer || do_errormacro) {
                break;
            }

            if (lex != 0) {
                lexseq_instail(result, lex);
            }

            // Handle recursion.  XXX - while the LRM doesn't
            // specifically mention this, recursion invocations
            // are typically of the form MACRONAME(%REMAINING)
            // and that is the only thing we handle here -- no
            // other arguments may be specified. - XXX
            if (do_recursion) {
                scopectx_t subscope;
                nameref_t *cformal;
                lextype_t lt;
                lexseq_t newremain;
                int which;

                lex = bodynext;
                if (lex == 0) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                bodynext = lexeme_next(lex);
                lt = lexeme_boundtype(lex);
                for (which = 0; which < 3; which++) {
                    if (lt == openers[which])
                        break;
                }
                if (which >= 3) {
                    do_errormacro = 1;
                    break;
                }
                lex = bodynext;
                if (lex == 0) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                bodynext = lexeme_next(lex);
                if (lexeme_boundtype(lex) != LEXTYPE_LXF_REMAINING) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                lex = bodynext;
                if (lex == 0) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                bodynext = lexeme_next(lex);;
                if (lexeme_boundtype(lex) != closers[which]) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                lexseq_init(&newremain);
                if (lexseq_length(remaining) > 0) {
                    lextype_t terms[1] = { LEXTYPE_DELIM_COMMA };
                    lexseq_t rresult;
                    int ok;

                    lexseq_init(&rresult);
                    cformal = namereflist_head(&macro->plist);
                    lexseq_copy(lctx, &newremain, remaining);
                    subscope = scope_copy(expscope, 0);

                    while (cformal != 0) {
                        strdesc_t *cfname = name_string(cformal->np);
                        np = name_search(subscope, cfname->ptr, cfname->len, 0);
                        if (np == 0) {
                            /* XXX error condition - should never happen */
                        }
                        lexseq_free(lctx, name_extraspace(np));
                        if (lexseq_length(&newremain) != 0) {
                            lexseq_t *val = name_extraspace(np);
                            parse_lexeme_seq(pctx, &newremain, QL_MACRO,
                                             terms, 1, val, &lt);
                        }
                        cformal = cformal->tq_next;
                    } /* walk the formals */
                    // Hook the name table into the hierarchy and recurse.
                    scope_setparent(subscope, scope_getparent(expscope));
                    ok = prepare_body(pctx, subscope, macro, curdepth+1,
                                      actcount-namereflist_length(&macro->plist),
                                      &newremain, &rresult, &do_errormacro);
                    scope_end(subscope);
                    lexseq_free(lctx, &newremain);
                    if (ok) {
                        lexseq_append(result, &rresult);
                    } else {
                        lexseq_free(lctx, &rresult);
                    }

                } /* %REMAINING is non-null */

            } /* if (do_recursion) */

        } /* for-walk through the body */

        if (do_exitmacro || do_errormacro ||
            macro->type != MACRO_ITER ||
            lexseq_length(remaining) == 0) {
            break;
        }

        curdepth += 1;
        lex = parser_punct_separator(pctx);
        if (lex != 0) {
            lexseq_instail(result, lex);
        }

    } /* while-1 iteration loop */

    if (do_errormacro) {
        if (errorout != 0) {
            *errorout = 1;
        }
        lexseq_free(lctx, result);
        return 0;
    }

    if (macro->type == MACRO_ITER) {
        lex = parser_punct_grouper(pctx, 1);
        if (lex != 0) {
            lexseq_instail(result, lex);
        }
    }

    return 1;

} /* prepare_body */

/*
 * macro_expand
 *
 * Expands a macro.
 */
static int
macro_expand (parse_ctx_t pctx, name_t *macroname,
              lexseq_t *result)
{
    struct macrodecl_s *macro = name_extraspace(macroname);
    lexctx_t lctx = parser_lexmemctx(pctx);
    lextype_t lt;
    lexeme_t *lex;
    lexseq_t extras;
    name_t *np;
    scopectx_t expscope;
    int which, ok;
    int nactuals;
    lextype_t terms[3];
    static strdesc_t comma = STRDEF(",");

    if (macro == 0) {
        /* XXX error condition */
        return 1;
    }

    // For keyword macros, prime the scope with the declared
    // formals, so we can inherit the default values.
    if (macro->type == MACRO_KWD) {
        expscope = scope_copy(macro->ptable, 0);
    } else {
        expscope = scope_begin(scope_namectx(parser_scope_get(pctx)), 0);
    }

    nactuals = 0;
    lexseq_init(&extras);

    // Simple macros with no formal arguments get no processing
    // of parameter lists whatsoever.
    if (macro->type == MACRO_SIMPLE && namereflist_length(&macro->plist) == 0) {
        expscope = 0;
        which = 3;
    } else {

        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (macro->type == MACRO_KWD) {
            if (lt != LEXTYPE_DELIM_LPAR) {
                /* XXX error condition */
                parser_insert(pctx, lex);
                return 1;
            }
            which = 0;
            lexeme_free(lctx, lex);
        } else {
            for (which = 0; lt != openers[which] && which < 3; which++);
            if (which >= 3 && namereflist_length(&macro->plist) > 0) {
                /* XXX error condition */
                parser_insert(pctx, lex);
                return 1;
            }
            if (which >= 3) {
                parser_insert(pctx, lex);
            } else {
                lexeme_free(lctx, lex);
            }
        }
    }

    // If we had a match on an opener, process
    // the actual parameters.
    if (which < 3) {

        lexeme_t *extralast;
        nameref_t *formal;
        lexseq_t val;

        terms[0] = LEXTYPE_DELIM_COMMA;
        terms[1] = closers[which];

        extralast = 0;
        formal = namereflist_head(&macro->plist);

        while (1) {
            // Keyword macro actuals are of the form name=value
            // For positionals, grab the next formal-parameter name,
            // or set np to NULL to add the actual to %REMAINING.
            if (macro->type == MACRO_KWD) {
                lt = parser_next(pctx, QL_NAME, &lex);
                if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
                    /* XXX error condition */
                    lexeme_free(lctx, lex);
                    break;
                }
                np = name_search(macro->ptable, lex->text.ptr, lex->text.len, 0);
                if (np == 0) {
                    /* XXX error condition */
                }
                lexeme_free(lctx, lex);
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
                    /* XXX error condition */
                    break;
                }
            } else if (nactuals < namereflist_length(&macro->plist)) {
                np = formal->np;
                formal = formal->tq_next;
            } else {
                np = 0;
            }
            lexseq_init(&val);
            // Now parse the actual-parameter, which can be an expression
            if (!parse_lexeme_seq(pctx, 0, QL_NAME, terms, 2, &val, &lt)) {
                lexseq_free(lctx, &val);
                break;
            }

            if (np == 0) {
                if (lexseq_length(&extras) > 0) {
                    lexseq_instail(&extras,
                                   lexeme_create(lctx, LEXTYPE_DELIM_COMMA, &comma));
                }
                lexseq_append(&extras, &val);
            } else {
                name_t *actual;
                // Associate the actual with the formal.  For keyword
                // macros, the scope_copy() above sets a special "no check"
                // flag that allows each name to be redeclared once.
                // name_declare() clears this flag, so we can catch genuine
                // redeclarations.
                actual = macparam_special(expscope, name_string(np), &val);
                if (actual == 0) {
                    /* XXX error condition */
                }
                lexseq_free(lctx, &val);
            }

            nactuals += 1;

            if (lt == closers[which]) {
                break;
            }

            if (lt != LEXTYPE_DELIM_COMMA) {
                /* XXX error condition */
                break;
            }

        } /* while (1) */

        if (lt != closers[which]) {
            /* XXX error condition */
            lexseq_free(lctx, &extras);
            scope_end(expscope);
            return 1;
        }

        if (nactuals < namereflist_length(&macro->plist)) {
            name_t *anp;
            while (formal != 0) {
                anp = macparam_special(expscope, name_string(formal->np), 0);
                if (anp == 0) {
                    /* XXX error condition */
                }
                formal = formal->tq_next;
            }
        }

    } /* if which < 3 */

    scope_setparent(expscope, parser_scope_get(pctx));

    ok = prepare_body(pctx, expscope, macro, 0, nactuals,
                      &extras, result, 0);

    if (!ok) {
        lexseq_free(lctx, result);
    }
    scope_end(expscope);

    return 1;

} /* macro_expand */