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
    lexeme_t *body;
    macrotype_t type;
    int paramcount, iparamcount;
    struct macparam_s *plist, *ilist;
    scopectx_t ptable;
};

static int parse_KEYWORDMACRO(parse_ctx_t pctx), parse_MACRO(parse_ctx_t pctx);

static name_t macro_names[] = {
    KWDDEF("KEYWORDMACRO", parse_KEYWORDMACRO),
    KWDDEF("MACRO", parse_MACRO),
    LEXDEF("%EXITMACRO", LEXTYPE_OP_EXITMACRO, NAME_M_OPERATOR),
    LEXDEF("%EXITITERATION", LEXTYPE_OP_EXITITER, NAME_M_OPERATOR),
    LEXDEF("%ERRORMACRO", LEXTYPE_OP_ERRORMACRO, NAME_M_OPERATOR)
};

struct macparam_s *freelist;

static lextype_t openers[] = { LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LBRACK,
                               LEXTYPE_DELIM_LANGLE };
static lextype_t closers[] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RBRACK,
                               LEXTYPE_DELIM_RANGLE };

static strdesc_t pct_remaining = STRDEF("%REMAINING"),
                 pct_count     = STRDEF("%COUNT"),
                 pct_length    = STRDEF("%LENGTH");

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
    name_t *mnp;
    scopectx_t pscope;
    struct macparam_s *param, *lastp;
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

    while (1) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_TEXT || lexeme_boundtype(lex) != LEXTYPE_IDENT) {
            /* XXX error condition */
            break;
        }
        mnp = name_declare(pscope, lex->text.ptr, lex->text.len,
                           NAMETYPE_MAC_PARAM, 0);
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
        lt = parser_next(pctx, &lex);
        if (assign_allowed && lt == LEXTYPE_OP_ASSIGN) {
            quotelevel_t oldql;
            lexeme_free(lex);
            oldql = parser_set_quotelevel(pctx, QL_MACRO);
            if (!parse_lexeme_seq(pctx, 0, terms, 2, &lex, &lt)) {
                /* XXX error condition */
                break;
            }
            parser_set_quotelevel(pctx, oldql);
            mnp->namedata.ptr = lex;
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
 * define_macro
 *
 * Common logic for macro declarations.
 *
 * MACRO macro-name { (param,...) } { [param,...] } = {stuff} % {,...}
 * KEYWORDMACRO macro-name { (param{=defval},...) } = {stuff} % {,...}
 */
static int
define_macro (parse_ctx_t pctx, int is_kwdmacro)
{
    int skip_to_end = 0;
    quotelevel_t oldql;
    lexeme_t *lex, *body;
    lextype_t lt;
    name_t *np;
    scopectx_t scope, ntbl;
    struct macparam_s *nlst, *clst;
    int normcount, condcount;
    macrotype_t mtype = (is_kwdmacro ? MACRO_KWD : MACRO_UNK);
    
    if (!parser_decl_ok(pctx, &scope)) {
        /* XXX error condition */
        skip_to_end = 1;
    }

    oldql = parser_set_quotelevel(pctx, QL_NAME);

    while (1) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_TEXT || lexeme_boundtype(lex) != LEXTYPE_IDENT) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        np = lexeme_nameval(lex, scope);
        lexeme_free(lex);
        if (np->nameflags & NAME_M_RESERVED) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        if (np->nametype != NAMETYPE_UNDECLARED && np->namescope == scope) {
            /* XXX redeclaration error */
            skip_to_end = 1;
        }
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_DELIM_LPAR &&
            lt != LEXTYPE_OP_ASSIGN &&
            (is_kwdmacro || lt == LEXTYPE_DELIM_LBRACK)) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        ntbl = 0;
        nlst = clst = 0;
        body = 0;
        normcount = condcount = 0;
        if (lt == LEXTYPE_DELIM_LPAR) {
            if (!parse_paramlist(pctx, scope, is_kwdmacro,
                                 LEXTYPE_DELIM_RPAR, &ntbl,
                                 (!is_kwdmacro ? &nlst : 0),
                                 &normcount)) {
                skip_to_end = 1;
            }
            lexeme_free(lex);
            if (normcount == 0) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            lt = parser_next(pctx, &lex);
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
            lexeme_free(lex);
            lt = parser_next(pctx, &lex);
            mtype = (condcount == 0 ? MACRO_COND : MACRO_ITER);
        }
        if (lt == LEXTYPE_OP_ASSIGN) {
            lexeme_t *last;
            parser_set_quotelevel(pctx, QL_MACRO);
            parser_incr_erroneof(pctx);
            lexeme_free(lex);
            for (lt = parser_next(pctx, &lex);
                 lt != LEXTYPE_DELIM_PERCENT;
                 lt = parser_next(pctx, &lex)) {
                if (lt == LEXTYPE_END || lt == LEXTYPE_NONE) {
                    /* XXX error condition */
                    skip_to_end = 1;
                    break;
                }
                lex->next = 0;
                if (body == 0) {
                    body = last = lex;
                } else {
                    last->next = lex;
                    last = lex;
                }
            }
            parser_decr_erroneof(pctx);
            parser_set_quotelevel(pctx, QL_NAME);
        }
        lexeme_free(lex);
        if (!skip_to_end) {
            struct macrodecl_s *macro = malloc(sizeof(struct macrodecl_s));
            if (macro == 0) {
                /* XXX error condition */
            } else {
                data_t namedata = { 0 };
                macro->type = mtype;
                macro->body = body;
                macro->paramcount = normcount;
                scope_setparent(ntbl, 0);
                macro->ptable = ntbl;
                macro->plist = nlst;
                macro->ilist = clst;
                macro->iparamcount = condcount;
                namedata.ptr = macro;
                name_declare(scope, np->name, np->namelen, NAMETYPE_MACRO, &namedata);
                {
                    extern void PRINTLEX(lexeme_t *lex), PRINTCR(void);
                    lexeme_t *l;
                    printf("\n---Definition of %*.*s", (int)np->namelen,
                           (int)np->namelen, np->name); PRINTCR();
                    for (l = body; l != 0; l = l->next) { PRINTLEX(l); }
                    printf("\n---end of expansion---"); PRINTCR();
                }
            }
        } else {
            lexseq_free(body);
            scope_end(ntbl);
            break;
        }

        lt = parser_next(pctx, &lex);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            break;
        }

    } /* while 1 */

    lexeme_free(lex);
    parser_set_quotelevel(pctx, oldql);
    return 1;

} /* define_macro */

static int parse_KEYWORDMACRO (parse_ctx_t pctx) { return define_macro(pctx, 1); }
static int parse_MACRO (parse_ctx_t pctx) { return define_macro(pctx, 0); }

/*
 * Handle the macro-functions that act like variables.
 */
static int
macro_special (lexeme_t *orig, lexeme_t *remaining, unsigned int curdepth,
               unsigned int actcount, lexeme_t **result)
{
    strdesc_t *str;
    lexeme_t *lex;

    if (lexeme_boundtype(orig) != LEXTYPE_IDENT) {
        return 0;
    }
    if (strings_eql(&orig->text, &pct_count)) {
        str = string_printf(0, "%u", curdepth);
        lex = lexeme_create(LEXTYPE_NUMERIC, str);
        string_free(str);
    } else if (strings_eql(&orig->text, &pct_remaining)) {
        lex = lexseq_copy(remaining);
    } else if (strings_eql(&orig->text, &pct_length)) {
        str = string_printf(0, "%u", actcount);
        lex = lexeme_create(LEXTYPE_NUMERIC, str);
        string_free(str);
    } else {
        return 0;
    }

    *result = lex;
    return 1;

}

/*
 * prepare_body
 *
 * Builds a lexeme sequence with the expansion of the body of
 * a macro.
 */
static lexeme_t *
prepare_body (parse_ctx_t pctx, scopectx_t expscope, struct macrodecl_s *macro,
              unsigned int curdepth, unsigned int actcount, lexeme_t *remaining,
              int *errorout)
{
    lexeme_t *result, *rlast, *lex, *bodynext, *special;
    name_t *np;
    int do_exitmacro, do_exititer, do_errormacro, do_recursion;

    result = rlast = 0;
    do_recursion = do_exitmacro = do_exititer = do_errormacro = 0;

    while (1) {

        // Associate the iterative-formals with any actuals
        if (macro->type == MACRO_ITER) {
            struct macparam_s *iformal;
            lexeme_t *pseq;
            lextype_t lt, terms[1] = { LEXTYPE_DELIM_COMMA };
            for (iformal = macro->ilist; iformal != 0; iformal = iformal->next) {
                np = name_search(expscope, iformal->np->name, iformal->np->namelen, 1);
                if (np->namescope != expscope) {
                    np = name_declare(expscope, iformal->np->name, iformal->np->namelen, NAMETYPE_MAC_PARAM, 0);
                }
                np->nametype = NAMETYPE_MAC_PARAM;
                parse_lexeme_seq(pctx, &remaining, terms, 1, &pseq, &lt);
                np->namedata.ptr = pseq;
            }
        }

        // Copy the body text.  Handle %COUNT, %LENGTH, and %REMAINING
        // specially, and check for recursion.  Also check for the
        // exiting lexical functions (which are classified as operators
        // since we check them in-line rather than calling functions).
        for (lex = macro->body; lex != 0; lex = bodynext) {
            bodynext = lex->next;
            lex = lexeme_copy(lex);
            if (lexeme_boundtype(lex) == LEXTYPE_IDENT) {
                if (macro_special(lex, remaining, curdepth, actcount, &special)) {
                    lexeme_free(lex);
                    lex = special;
                } else {
                    np = name_search(expscope, lex->text.ptr, lex->text.len, 0);
                    if (np != 0) {
                        if (np->nametype == NAMETYPE_MACRO &&
                            np->namedata.ptr == macro) {
                            if (macro->type != MACRO_COND) {
                                /* XXX error condition */
                                lexseq_free(result);
                                return 0;
                            }
                            do_recursion = 1;
                            lex = 0;
                        } else if ((np->nameflags & NAME_M_OPERATOR) &&
                                   np->nametype == NAMETYPE_LEXFUNC) {
                            lextype_t lt = (lextype_t) np->namedata.val_signed;
                            if (lt == LEXTYPE_OP_EXITMACRO) {
                                do_exitmacro = 1;
                            } else if (lt == LEXTYPE_OP_EXITITER) {
                                do_exititer = 1;
                            } else if (lt == LEXTYPE_OP_ERRORMACRO) {
                                // process the parameters, set the result
                                do_errormacro = 1;
                            }
                        } else if (np->namescope == expscope) {
                            lexeme_free(lex);
                            lex = lexseq_copy(np->namedata.ptr);
                        }
                    }
                }
            }

            if (do_exitmacro || do_exititer || do_errormacro) {
                break;
            }

            // Append to the result - could be a sequence of lexemes
            while (lex != 0) {
                if (result == 0) {
                    result = rlast = lex;
                } else {
                    rlast->next = lex;
                    rlast = lex;
                }
                lex = lex->next;
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
                lexeme_t *newremain;
                int which;

                lex = bodynext;
                if (lex == 0) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }
                bodynext = lex->next;
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
                bodynext = lex->next;
                if (lexeme_boundtype(lex) != LEXTYPE_IDENT ||
                    !strings_eql(&lex->text, &pct_remaining)) {
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
                bodynext = lex->next;
                if (lexeme_boundtype(lex) != closers[which]) {
                    /* XXX error condition */
                    do_errormacro = 1;
                    break;
                }

                if (remaining != 0) {
                    lextype_t terms[1] = { LEXTYPE_DELIM_COMMA };
                    cformal = macro->plist;
                    newremain = remaining;
                    subscope = scope_copy(expscope, 0);

                    while (cformal != 0) {
                        np = name_search(subscope, cformal->np->name,
                                         cformal->np->namelen, 0);
                        if (np == 0) {
                            /* XXX error condition - should never happen */
                        }
                        lexeme_free(np->namedata.ptr);
                        if (newremain == 0) {
                            np->namedata.ptr = 0;
                        } else {
                            parse_lexeme_seq(pctx, &newremain, terms, 1, &lex, &lt);
                            np->namedata.ptr = lex;
                        }
                        cformal = cformal->next;
                    } /* walk the formals */
                    // Hook the name table into the hierarchy and recurse.
                    scope_setparent(subscope, scope_getparent(expscope));
                    lex = prepare_body(pctx, subscope, macro,
                                   curdepth+1, actcount-macro->paramcount,
                                   newremain, &do_errormacro);
                    scope_end(subscope);
                    while (lex != 0) {
                        if (result == 0) {
                            result = rlast = lex;
                        } else {
                            rlast->next = lex;
                            rlast = lex;
                        }
                        lex = lex->next;
                    }

                } /* %REMAINING is non-null */

            } /* if (do_recursion) */

        } /* for-walk through the body */

        if (do_exitmacro || do_errormacro ||
            macro->type != MACRO_ITER || remaining == 0) {
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

    return result;

} /* prepare_body */

/*
 * macro_expand
 *
 * Expands a macro.
 */
int
macro_expand (parse_ctx_t pctx, scopectx_t curscope, name_t *macroname)
{
    struct macrodecl_s *macro = macroname->namedata.ptr;
    lextype_t lt;
    lexeme_t *lex, *extras;
    name_t *np;
    scopectx_t expscope;
    int which;
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
    extras = 0;

    // Simple macros with no formal arguments get no processing
    // of parameter lists whatsoever.
    if (macro->type == MACRO_SIMPLE && macro->paramcount == 0) {
        expscope = 0;
        which = 3;
    } else {

        lt = parser_next(pctx, &lex);
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

        quotelevel_t oldql = parser_set_quotelevel(pctx, QL_NAME);
        lexeme_t *extralast;
        struct macparam_s *formal;

        terms[0] = LEXTYPE_DELIM_COMMA;
        terms[1] = closers[which];

        extralast = 0;
        formal = macro->plist;

        while (1) {
            // Keyword macro actuals are of the form name=value
            // For positionals, grab the next formal-parameter name,
            // or set np to NULL to add the actual to %REMAINING.
            if (macro->type == MACRO_KWD) {
                lt = parser_next(pctx, &lex);
                if (lt != LEXTYPE_TEXT || lexeme_boundtype(lex) != LEXTYPE_IDENT) {
                    /* XXX error condition */
                    lexeme_free(lex);
                    break;
                }
                np = name_search(macro->ptable, lex->text.ptr, lex->text.len, 0);
                if (np == 0) {
                    /* XXX error condition */
                }
                lexeme_free(lex);
                lt = parser_next(pctx, &lex);
                lexeme_free(lex);
                if (lt != LEXTYPE_OP_ASSIGN) {
                    /* XXX error condition */
                    break;
                }
            } else if (nactuals < macro->paramcount) {
                np = formal->np;
                formal = formal->next;
            } else {
                np = 0;
            }
            // Now parse the actual-parameter, which can be an expression
            if (!parse_lexeme_seq(pctx, 0, terms, 2, &lex, &lt)) {
                break;
            }

            if (np == 0) {
                lexeme_t *llast, *l;
                // Add to %REMAINING
                for (llast = l = lex; l != 0; llast = l, l = l->next);
                if (extras == 0) {
                    extras = lex;
                    extralast = llast;
                } else {
                    extralast->next = lexeme_create(LEXTYPE_DELIM_COMMA, &comma);
                    extralast = extralast->next;
                    extralast->next = lex;
                    extralast = llast;
                }
            } else {
                name_t *actual;
                // Associate the actual with the formal.  For keyword
                // macros, the scope_copy() above sets a special "no check"
                // flag that allows each name to be redeclared once.
                // name_declare() clears this flag, so we can catch genuine
                // redeclarations.
                actual = name_declare(expscope, np->name, np->namelen,
                                      NAMETYPE_MAC_PARAM, 0);
                if (actual == 0) {
                    /* XXX error condition */
                } else {
                    actual->namedata.ptr = lex;
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

        parser_set_quotelevel(pctx, oldql);

        if (lt != closers[which]) {
            /* XXX error condition */
            lexseq_free(extras);
            scope_end(expscope);
            return 1;
        }

        if (nactuals < macro->paramcount) {
            while (formal != 0) {
                name_declare(expscope, formal->np->name, formal->np->namelen,
                             NAMETYPE_MAC_PARAM, 0);
                formal = formal->next;
            }
        }

    } /* if which < 3 */

    scope_setparent(expscope, curscope);
    lex = prepare_body(pctx, expscope, macro, 0, nactuals, extras, 0);

    {
        extern void PRINTLEX(lexeme_t *lex), PRINTCR(void);
        lexeme_t *l;
        printf("\n---Expansion of %*.*s", (int)macroname->namelen,
               (int)macroname->namelen, macroname->name); PRINTCR();
        for (l = lex; l != 0; l = l->next) { PRINTLEX(l); }
        printf("\n---end of expansion---"); PRINTCR();
    }

    parser_insert(pctx, lex);
    scope_end(expscope);

    return 1;

} /* macro_expand */
