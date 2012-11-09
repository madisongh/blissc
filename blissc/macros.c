//
//  macros.c
//  blissc
//
//  Created by Matthew Madison on 11/6/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "parser.h"
#include "lexer.h"
#include "nametable.h"
#include "lexeme.h"
#include "strings.h"
#include "macros.h"

struct macrodecl_s {
    lexeme_t *body;
    int kwdmacro;
    int paramcount, condparamcount;
    scopectx_t ptable, ctable;
};

static int parse_KEYWORDMACRO(parse_ctx_t pctx), parse_MACRO(parse_ctx_t pctx);

static name_t macro_names[] = {
    KWDDEF("KEYWORDMACRO", parse_KEYWORDMACRO),
    KWDDEF("MACRO", parse_MACRO)
};

/*
 * macros_init
 *
 * Initialization routine.
 */
void
macros_init (scopectx_t kwdscope)
{
    int i;
    for (i = 0; i < sizeof(macro_names)/sizeof(macro_names[0]); i++) {
        name_insert(kwdscope, &macro_names[i]);
    }
    
} /* macros_init */

/*
 * parse_paramlist
 *
 * Parse a macro parameter list definition.
 */
static int
parse_paramlist (parse_ctx_t pctx, scopectx_t curscope,
                 int assign_allowed, lextype_t closer,
                 scopectx_t *ptable, int *pcount)
{
    lexeme_t *lex, *head, *last;
    lextype_t lt;
    name_t *mnp;
    scopectx_t pscope;
    int count = 0;

    pscope = scope_begin(curscope);
    head = last = 0;
    lt = parser_next(pctx, &lex);

    while (1) {
        if (lt != LEXTYPE_IDENT) {
            /* XXX error condition */
            break;
        }
        mnp = name_declare(pscope, lex->text.ptr, lex->text.len,
                           NAMETYPE_MAC_PARAM, 0);
        if (mnp == 0) {
            /* XXX error condition */
            break;
        }
        lexeme_free(lex);
        count += 1;
        lt = parser_next(pctx, &lex);
        if (assign_allowed && lt == LEXTYPE_OP_ASSIGN) {
            quotelevel_t oldql;
            lexeme_free(lex);
            oldql = parser_set_quotelevel(pctx, QL_MACRO);
            lt = parser_next(pctx, &lex);
            parser_set_quotelevel(pctx, oldql);
            lex->next = 0;
            mnp->namedata.ptr = lex;
            lt = parser_next(pctx, &lex);
        }
        lexeme_free(lex);
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
    }
    return 1;

} /* parse_paramlist */

/*
 * free_paramlist
 *
 * Free up memory for a parameter list.
 */
static void
free_paramlist (void *plist)
{
    
}

/*
 * define_macro
 *
 * Common logic for macro declarations.
 */
static int
define_macro (parse_ctx_t pctx, int is_kwdmacro)
{
    int skip_to_end = 0;
    quotelevel_t oldql;
    lexeme_t *lex, *body;
    lextype_t lt;
    name_t *np;
    scopectx_t scope, ntbl, ctbl;
    int normcount, condcount;
    
    if (!parser_decl_ok(pctx, &scope)) {
        /* XXX error condition */
        skip_to_end = 1;
    }

    oldql = parser_set_quotelevel(pctx, QL_NAME);

    while (1) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_IDENT) {
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
        ntbl = ctbl = 0;
        body = 0;
        normcount = condcount = 0;
        if (lt == LEXTYPE_DELIM_LPAR) {
            if (!parse_paramlist(pctx, scope, is_kwdmacro,
                                 LEXTYPE_DELIM_RPAR, &ntbl, &normcount)) {
                skip_to_end = 1;
            }
            lexeme_free(lex);
            lt = parser_next(pctx, &lex);
        }
        if (lt == LEXTYPE_DELIM_LBRACK) {
            if (is_kwdmacro) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            if (!parse_paramlist(pctx, scope, 0,
                                 LEXTYPE_DELIM_RBRACK, &ctbl, &condcount)) {
                skip_to_end = 1;
            }
            lexeme_free(lex);
            lt = parser_next(pctx, &lex);
        }
        if (lt == LEXTYPE_OP_ASSIGN) {
            lexeme_t *last;
            parser_set_quotelevel(pctx, QL_MACRO);
            parser_incr_erroneof(pctx);
            lexeme_free(lex);
            for (lt = parser_next(pctx, &lex); lt != LEXTYPE_DELIM_PERCENT;
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
                macro->kwdmacro = is_kwdmacro;
                macro->body = body;
                macro->paramcount = normcount;
                macro->ptable = ntbl;
                macro->ctable = ctbl;
                macro->condparamcount = condcount;
                namedata.ptr = macro;
                name_declare(scope, np->name, np->namelen, NAMETYPE_MACRO, &namedata);
            }
        } else {
            lexseq_free(body);
            scope_end(ctbl);
            scope_end(ntbl);
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
