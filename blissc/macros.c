//
//  macros.c
//  blissc
//
//  Created by Matthew Madison on 11/6/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "parser.h"
#include "lexer.h"
#include "nametable.h"
#include "lexeme.h"
#include "strings.h"
#include "macros.h"

#define PARAM_ALLOCOUNT 64

struct macparam_s {
    struct macparam_s *next;
    name_t *np;
};

typedef enum {
    MACRO_UNK, MACRO_SIMPLE, MACRO_COND, MACRO_ITER, MACRO_KWD
} macrotype_t;

struct macrodecl_s {
    lexseq_t body;
    macrotype_t type;
    int paramcount, iparamcount;
    struct macparam_s *plist, *ilist;
    scopectx_t ptable;
};

static name_t macro_names[] = {
    NAMEDEF("KEYWORDMACRO", LEXTYPE_DCL_KEYWORDMACRO, NAME_M_RESERVED),
    NAMEDEF("MACRO", LEXTYPE_DCL_MACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITMACRO", LEXTYPE_LXF_EXITMACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITITERATION", LEXTYPE_LXF_EXITITER, NAME_M_RESERVED),
    NAMEDEF("%ERRORMACRO", LEXTYPE_LXF_ERRORMACRO, NAME_M_RESERVED),
    NAMEDEF("%REMAINING", LEXTYPE_LXF_REMAINING, NAME_M_RESERVED),
    NAMEDEF("%COUNT", LEXTYPE_LXF_COUNT, NAME_M_RESERVED),
    NAMEDEF("%LENGTH", LEXTYPE_LXF_LENGTH, NAME_M_RESERVED)
};

struct macparam_s *freelist;

static lextype_t openers[] = { LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LBRACK,
                               LEXTYPE_DELIM_LANGLE };
static lextype_t closers[] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RBRACK,
                               LEXTYPE_DELIM_RANGLE };

static int macro_expand(parse_ctx_t pctx, name_t *macroname,
                        lexseq_t *result);
static void macro_freedata(name_t *np);
static int macro_copydata(name_t *dst, name_t *src);
static void macparam_freedata(name_t *np);
static int macparam_copydata(name_t *dst, name_t *src);


/*
 * macro_bind
 */
static int
macro_bind (void *ctx, quotelevel_t ql, quotemodifier_t qm,
            lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    parse_ctx_t pctx = ctx;
    name_t *np = lexeme_ctx_get(lex);
    strdesc_t namedsc;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    strdesc_init(&namedsc, np->name, np->namelen);

    switch (qm) {
        case QM_QUOTE:
            lex->type = LEXTYPE_UNBOUND;
            return 0;
        case QM_EXPAND:
            lexeme_free(lex);
            return macro_expand(pctx, np, result);
        case QM_UNQUOTE:
            lex->type = name_type(np);
            return 0;
        default:
            break;
    }

    // no quote modifier

    if (ql == QL_MACRO) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexeme_free(lex);
    return macro_expand(pctx, np, result);

} /* macro_bind */

/*
 * macparam_bind
 */
static int
macparam_bind (void *ctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    name_t *np = lexeme_ctx_get(lex);
    lexseq_t tmpseq;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }
    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexseq_init(&tmpseq);
    lexseq_copy(&tmpseq, name_data(np));
    lexseq_append(result, &tmpseq);
    lexeme_free(lex);

    return 1;

} /* macparam_bind */

/*
 * macros_init
 *
 * Initialization routine.
 */
void
macros_init (scopectx_t kwdscope)
{
    int i;
    struct macparam_s *p, *l;
    for (i = 0; i < sizeof(macro_names)/sizeof(macro_names[0]); i++) {
        name_insert(kwdscope, &macro_names[i]);
    }
    freelist = malloc(sizeof(struct macparam_s) * PARAM_ALLOCOUNT);
    for (l = freelist, p = freelist + 1, i = 1; i < PARAM_ALLOCOUNT;
         i+= 1, l = p, p = p + 1) {
        l->next = p;
    }
    l->next = 0;

    lextype_register(LEXTYPE_NAME_MACRO, macro_bind);
    lextype_register(LEXTYPE_NAME_MAC_PARAM, macparam_bind);
    nametype_dataop_register(LEXTYPE_NAME_MACRO, macro_freedata,
                               macro_copydata);
    nametype_dataop_register(LEXTYPE_NAME_MAC_PARAM, macparam_freedata,
                               macparam_copydata);

} /* macros_init */

/*
 * macparam_alloc
 *
 * Allocate macro parameter structure.
 */
static struct macparam_s *
macparam_alloc (void)
{
    struct macparam_s *p, *l;
    int i;

    if (freelist == 0) {
        freelist = malloc(sizeof(struct macparam_s) * PARAM_ALLOCOUNT);
        for (l = freelist, p = freelist + 1, i = 1; i < PARAM_ALLOCOUNT;
             i += 1, l = p, p = p + 1) {
            l->next = p;
        }
        l->next = 0;
    }
    p = freelist;
    freelist = freelist->next;
    p->next = 0;
    p->np = 0;
    return p;

} /* macparam_alloc */

/*
 * macparams_free
 *
 * Free up a list of macparam structures.
 */
static void
macparams_free (struct macparam_s *plist)
{
    struct macparam_s *p, *n;
    for (p = plist; p != 0; p = n) {
        n = p->next;
        p->next = freelist;
        freelist = p;
    }
} /* macparams_free */

/*
 * macparam_freedata
 *
 * Frees up a macro parameter (in the name structure).
 */
static void
macparam_freedata (name_t *np)
{
    lexseq_t *seq = name_data(np);
    lexseq_free(seq);

} /* macparam_freedata */

/*
 * macparam_copydata
 *
 * Copies a macro parameter.
 */
static int
macparam_copydata (name_t *dst, name_t *src)
{
    lexseq_t *src_seq, *dst_seq;
    src_seq = name_data(src);
    dst_seq = name_data(dst);
    lexseq_init(dst_seq);
    lexseq_copy(dst_seq, src_seq);
    return 1;
}

/*
 * macro_freedata
 *
 * Releases a macro's storage.  Called by name_free().
 */
static void
macro_freedata (name_t *np)
{
    struct macrodecl_s *macro = *(struct macrodecl_s **)name_data(np);
    macparams_free(macro->plist);
    macparams_free(macro->ilist);
    lexseq_free(&macro->body);
    free(macro);
    
} /* macro_freedata */

/*
 * macro_copydata
 *
 * Copies a macro's storage.  Called by name_copy().
 */
static int
macro_copydata (name_t *dst, name_t *src)
{
    struct macrodecl_s *srcm, *dstm;
    struct macparam_s *smp, *dmp, *dmpl;
    srcm = *(struct macrodecl_s **)name_data(src);
    dstm = malloc(sizeof(struct macrodecl_s));
    if (dstm == 0) return 0;
    memcpy(dstm, srcm, sizeof(struct macrodecl_s));
    lexseq_init(&dstm->body);
    lexseq_copy(&dstm->body, &srcm->body);
    dstm->plist = dstm->ilist = 0;
    for (smp = srcm->plist, dmp = dmpl = 0; smp != 0;
         dmpl = dmp, smp = smp->next) {
        dmp = macparam_alloc();
        memcpy(dmp, smp, sizeof(struct macparam_s));
        dmp->next = 0;
        if (dmpl == 0) {
            dstm->plist = dmp;
        } else {
            dmpl->next = dmp;
        }
    }

    for (smp = srcm->ilist, dmp = dmpl = 0; smp != 0;
         dmpl = dmp, smp = smp->next) {
        dmp = macparam_alloc();
        memcpy(dmp, smp, sizeof(struct macparam_s));
        dmp->next = 0;
        if (dmpl == 0) {
            dstm->ilist = dmp;
        } else {
            dmpl->next = dmp;
        }
    }
    *(struct macrodecl_s **)name_data(dst) = dstm;
    return 1;

} /* macro_copydata */

/*
 * parse_paramlist
 *
 * Parse a macro parameter list, for declarations.
 *
 * For keyword macros, handles the default value setting.
 *
 * For non-keyword macros, handles simple, conditional, and
 * iterative macro parameter lists.
 */
static int
parse_paramlist (parse_ctx_t pctx, scopectx_t curscope,
                 int assign_allowed, lextype_t closer,
                 scopectx_t *ptable, struct macparam_s **plist, int *pcount)
{
    lexeme_t *lex, *head, *last;
    lextype_t lt;
    strdesc_t *ltext;
    name_t *mnp;
    scopectx_t pscope;
    struct macparam_s *param, *lastp;
    lexseq_t nullseq;
    int count = 0;
    static lextype_t terms[] = {LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR};

    if (*ptable == 0) {
        pscope = scope_begin(curscope);
    } else {
        pscope = *ptable;
    }

    head = last = 0;
    if (plist != 0) {
        *plist = 0;
    }

    lexseq_init(&nullseq);

    while (1) {
        lt = parser_next(pctx, QL_NAME, &lex);
        if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
            /* XXX error condition */
            break;
        }
        ltext = lexeme_text(lex);
        mnp = name_declare(pscope, ltext->ptr, ltext->len,
                           LEXTYPE_NAME_MAC_PARAM,
                           &nullseq, sizeof(nullseq));
        if (mnp == 0) {
            /* XXX error condition */
            break;
        }
        if (plist != 0) {
            param = macparam_alloc();
            param->np = mnp;
            if (*plist == 0) {
                *plist = lastp = param;
            } else {
                lastp->next = param;
                lastp = param;
            }
        }
        lexeme_free(lex);
        count += 1;
        lt = parser_next(pctx, QL_NAME, &lex);
        if (assign_allowed && lt == LEXTYPE_OP_ASSIGN) {
            lexseq_t *defval = name_data(mnp);
            lexeme_free(lex);
            lexseq_init(defval);
            if (!parse_lexeme_seq(pctx, 0, QL_MACRO, terms, 2, defval, &lt)) {
                /* XXX error condition */
                break;
            }
        } else {
            lexeme_free(lex);
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            break;
        }
    }

    if (lt == closer) {
        *pcount = count;
        *ptable = pscope;
    } else {
        scope_end(pscope);
        parser_skip_to_delim(pctx, closer);
        if (plist != 0) {
            macparams_free(*plist);
            *plist = 0;
        }
    }
    return 1;

} /* parse_paramlist */

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
    lexseq_t body;
    lextype_t lt;
    strdesc_t *ltext;
    name_t *np;
    scopectx_t ntbl;
    struct macparam_s *nlst, *clst;
    int normcount, condcount;
    int is_kwdmacro = (curlt == LEXTYPE_DCL_KEYWORDMACRO);
    macrotype_t mtype;

    while (1) {
        lt = parser_next(pctx, QL_NAME, &lex);
        if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        ltext = lexeme_text(lex);
        np = name_search(scope, ltext->ptr, ltext->len, 1);
        lexeme_free(lex);
        if (np != 0) {
            if (name_flags(np) & NAME_M_RESERVED) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            if (name_type(np) != LEXTYPE_NAME && np->namescope == scope) {
                /* XXX redeclaration error */
                skip_to_end = 1;
            }
        }
        mtype = (is_kwdmacro ? MACRO_KWD : MACRO_UNK);
        lt = parser_next(pctx, QL_NAME, 0);
        if (lt != LEXTYPE_DELIM_LPAR &&
            lt != LEXTYPE_OP_ASSIGN &&
            (is_kwdmacro || lt == LEXTYPE_DELIM_LBRACK)) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        ntbl = 0;
        nlst = clst = 0;
        lexseq_init(&body);
        normcount = condcount = 0;
        if (lt == LEXTYPE_DELIM_LPAR) {
            if (!parse_paramlist(pctx, scope, is_kwdmacro,
                                 LEXTYPE_DELIM_RPAR, &ntbl,
                                 (!is_kwdmacro ? &nlst : 0),
                                 &normcount)) {
                skip_to_end = 1;
            }
            if (normcount == 0) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            if (!is_kwdmacro) {
                mtype = MACRO_SIMPLE;
            }
        }
        if (lt == LEXTYPE_DELIM_LBRACK) {
            if (is_kwdmacro) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            if (!parse_paramlist(pctx, scope, 0,
                                 LEXTYPE_DELIM_RBRACK,
                                 &ntbl, &clst, &condcount)) {
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            mtype = (condcount == 0 ? MACRO_COND : MACRO_ITER);
        }

        if (lt == LEXTYPE_OP_ASSIGN) {

            if (mtype == MACRO_UNK) {
                mtype = MACRO_SIMPLE;
            }

            parser_incr_erroneof(pctx);
            lexeme_free(lex);

            for (lt = parser_next(pctx, QL_MACRO, &lex);
                 lt != LEXTYPE_LXF_DELIM_PERCENT;
                 lt = parser_next(pctx, QL_MACRO, &lex)) {
                if (lt == LEXTYPE_END || lt == LEXTYPE_NONE) {
                    /* XXX error condition */
                    skip_to_end = 1;
                    break;
                }
                lex->next = 0;
                lexseq_instail(&body, lex);
            }
            parser_decr_erroneof(pctx);
        }

        if (!skip_to_end) {
            struct macrodecl_s *macro = malloc(sizeof(struct macrodecl_s));
            if (macro == 0) {
                /* XXX error condition */
            } else {
                macro->type = mtype;
                lexseq_init(&macro->body);
                lexseq_append(&macro->body, &body);
                macro->paramcount = normcount;
                scope_setparent(ntbl, 0);
                macro->ptable = ntbl;
                macro->plist = nlst;
                macro->ilist = clst;
                macro->iparamcount = condcount;
                name_declare(scope, np->name, np->namelen,
                             LEXTYPE_NAME_MACRO, &macro, sizeof(macro));
            }
        } else {
            lexseq_free(&body);
            scope_end(ntbl);
            break;
        }

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
    lexeme_t *lex, *bodynext;
    lexseq_t special;
    name_t *np;
    int do_exitmacro, do_exititer, do_errormacro, do_recursion;

    do_recursion = do_exitmacro = do_exititer = do_errormacro = 0;
    lexseq_init(&special);

    while (1) {

        // Associate the iterative-formals with any actuals
        if (macro->type == MACRO_ITER) {
            struct macparam_s *iformal;
            lexseq_t *pseq;
            lextype_t lt, terms[1] = { LEXTYPE_DELIM_COMMA };

            for (iformal = macro->ilist; iformal != 0; iformal = iformal->next) {
                np = name_search(expscope, iformal->np->name, iformal->np->namelen, 1);
                if (np->namescope != expscope) {
                    lexseq_t nullseq;
                    lexseq_init(&nullseq);
                    np = name_declare(expscope, iformal->np->name, iformal->np->namelen,
                                      LEXTYPE_NAME_MAC_PARAM, &nullseq, sizeof(nullseq));
                }
                pseq = name_data(np);
                lexseq_free(pseq);
                np->nametype = LEXTYPE_NAME_MAC_PARAM;
                parse_lexeme_seq(pctx, remaining, QL_MACRO, terms, 1, pseq, &lt);
            }
        }

        // Copy the body text, checking for recursion.

        for (lex = lexseq_head(&macro->body); lex != 0; lex = bodynext) {
            lextype_t lt;
            bodynext = lexeme_next(lex);
            lex = lexeme_copy(lex);
            lt = lexeme_boundtype(lex);
            if (lt == LEXTYPE_NAME) {
                np = name_search(expscope, lex->text.ptr, lex->text.len, 0);
                if (np != 0) {
                    lextype_t nlt = name_type(np);
                    struct macrodecl_s **mp = name_data(np);
                    if (nlt == LEXTYPE_NAME_MACRO && *mp == macro) {
                        if (macro->type != MACRO_COND) {
                            /* XXX error condition */
                            lexseq_free(result);
                            return 0;
                        }
                        do_recursion = 1;
                        lexeme_free(lex);
                        lex = 0;
                    } else if (np->namescope == expscope) {
                        lexseq_t valcopy;
                        lexeme_free(lex);
                        lexseq_init(&valcopy);
                        lexseq_copy(&valcopy, name_data(np));
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
                    lexeme_free(lex);
                    lex = 0;
                    break;
                case LEXTYPE_LXF_EXITITER:
                    do_exititer = 1;
                    lexeme_free(lex);
                    lex = 0;
                    break;
                case LEXTYPE_LXF_ERRORMACRO:
                    lexeme_free(lex);
                    lex = 0;
                    do_errormacro = 1;
                    break;
                case LEXTYPE_LXF_REMAINING: {
                    lexseq_t rcopy;
                    lexseq_init(&rcopy);
                    lexseq_copy(&rcopy, remaining);
                    lexseq_append(result, &rcopy);
                    lexeme_free(lex);
                    lex = 0;
                    break;
                }
                case LEXTYPE_LXF_COUNT:
                case LEXTYPE_LXF_LENGTH: {
                    strdesc_t * str;
                    str = string_printf(0, "%u",
                                        (lt == LEXTYPE_LXF_COUNT
                                         ? curdepth : actcount));
                    lexeme_free(lex);
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
                struct macparam_s *cformal;
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
                    cformal = macro->plist;
                    lexseq_copy(&newremain, remaining);
                    subscope = scope_copy(expscope, 0);

                    while (cformal != 0) {
                        np = name_search(subscope, cformal->np->name,
                                         cformal->np->namelen, 0);
                        if (np == 0) {
                            /* XXX error condition - should never happen */
                        }
                        lexseq_free(name_data(np));
                        if (lexseq_length(&newremain) != 0) {
                            lexseq_t *val = name_data(np);
                            parse_lexeme_seq(pctx, &newremain, QL_MACRO,
                                             terms, 1, val, &lt);
                        }
                        cformal = cformal->next;
                    } /* walk the formals */
                    // Hook the name table into the hierarchy and recurse.
                    scope_setparent(subscope, scope_getparent(expscope));
                    ok = prepare_body(pctx, subscope, macro,
                                   curdepth+1, actcount-macro->paramcount,
                                   &newremain, &rresult, &do_errormacro);
                    scope_end(subscope);
                    lexseq_free(&newremain);
                    if (ok) {
                        lexseq_append(result, &rresult);
                    } else {
                        lexseq_free(&rresult);
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
        // XXX - separator insertion here

    } /* while-1 iteration loop */

    if (do_errormacro) {
        if (errorout != 0) {
            *errorout = 1;
        }
        lexseq_free(result);
        return 0;
    }

    // XXX - grouper additions here

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
    struct macrodecl_s *macro = *(struct macrodecl_s **)name_data(macroname);
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
        expscope = scope_begin(0);
    }

    nactuals = 0;
    lexseq_init(&extras);

    // Simple macros with no formal arguments get no processing
    // of parameter lists whatsoever.
    if (macro->type == MACRO_SIMPLE && macro->paramcount == 0) {
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
            lexeme_free(lex);
        } else {
            for (which = 0; lt != openers[which] && which < 3; which++);
            if (which >= 3 && macro->paramcount > 0) {
                /* XXX error condition */
                parser_insert(pctx, lex);
                return 1;
            }
            if (which >= 3) {
                parser_insert(pctx, lex);
            } else {
                lexeme_free(lex);
            }
        }
    }

    // If we had a match on an opener, process
    // the actual parameters.
    if (which < 3) {

        lexeme_t *extralast;
        struct macparam_s *formal;
        lexseq_t val;

        terms[0] = LEXTYPE_DELIM_COMMA;
        terms[1] = closers[which];

        extralast = 0;
        formal = macro->plist;

        while (1) {
            // Keyword macro actuals are of the form name=value
            // For positionals, grab the next formal-parameter name,
            // or set np to NULL to add the actual to %REMAINING.
            if (macro->type == MACRO_KWD) {
                lt = parser_next(pctx, QL_NAME, &lex);
                if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
                    /* XXX error condition */
                    lexeme_free(lex);
                    break;
                }
                np = name_search(macro->ptable, lex->text.ptr, lex->text.len, 1);
                if (np == 0) {
                    /* XXX error condition */
                }
                lexeme_free(lex);
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
                    /* XXX error condition */
                    break;
                }
            } else if (nactuals < macro->paramcount) {
                np = formal->np;
                formal = formal->next;
            } else {
                np = 0;
            }
            lexseq_init(&val);
            // Now parse the actual-parameter, which can be an expression
            if (!parse_lexeme_seq(pctx, 0, QL_NAME, terms, 2, &val, &lt)) {
                lexseq_free(&val);
                break;
            }

            if (np == 0) {
                if (lexseq_length(&extras) > 0) {
                    lexseq_instail(&extras, lexeme_create(LEXTYPE_DELIM_COMMA, &comma));
                }
                lexseq_append(&extras, &val);
            } else {
                name_t *actual;
                // Associate the actual with the formal.  For keyword
                // macros, the scope_copy() above sets a special "no check"
                // flag that allows each name to be redeclared once.
                // name_declare() clears this flag, so we can catch genuine
                // redeclarations.
                actual = name_declare(expscope, np->name, np->namelen,
                                      LEXTYPE_NAME_MAC_PARAM, &val, sizeof(val));
                if (actual == 0) {
                    /* XXX error condition */
                }
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
            lexseq_free(&extras);
            scope_end(expscope);
            return 1;
        }

        if (nactuals < macro->paramcount) {
            lexseq_t nullseq;
            lexseq_init(&nullseq);
            while (formal != 0) {
                name_declare(expscope, formal->np->name, formal->np->namelen,
                             LEXTYPE_NAME_MAC_PARAM, &nullseq, sizeof(nullseq));
                formal = formal->next;
            }
        }

    } /* if which < 3 */

    scope_setparent(expscope, parser_scope_get(pctx));

    ok = prepare_body(pctx, expscope, macro, 0, nactuals,
                      &extras, result, 0);

    if (!ok) {
        lexseq_free(result);
    }
    scope_end(expscope);

    return 1;

} /* macro_expand */