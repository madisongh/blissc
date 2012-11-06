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

struct macroparam_s {
    struct macroparam_s *next;
    strdesc_t            name;
    strdesc_t            defval;
};

struct macrodecl_s {
    lexeme_t *body;
    int kwdmacro;
    int paramcount, condparamcount;
    struct macroparam_s *paramlist;
    struct macroparam_s *condparams;
};

static int parse_KEYWORDMACRO(parse_ctx_t pctx), parse_MACRO(parse_ctx_t pctx);

static name_t macro_names[] = {
    KWDDEF("KEYWORDMACRO", parse_KEYWORDMACRO),
    KWDDEF("MACRO", parse_MACRO)
};

/*
 * parse_paramlist
 *
 * Parse a macro parameter list definition.
 */
static int
parse_paramlist (parse_ctx_t pctx, lextype_t closer,
                 struct macroparam_s **plist, int *pcount)
{
    return 1;

} /* parse_paramlist */

/*
 * free_paramlist
 *
 * Free up memory for a parameter list.
 */
static void
free_paramlist (struct macroparam_s *plist)
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
    name_t *np;
    struct macroparam_s *normp, *condp;
    int normcount, condcount;
    
    if (!parser_decl_ok(pctx)) {
        /* XXX error condition */
        skip_to_end = 1;
    }

    oldql = parser_set_quotelevel(pctx, QL_NAME);

    while (1) {
        lex = parser_next(pctx);
        if (lex->type != LEXTYPE_IDENT) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        np = lex->data.ptr;
        lexeme_free(lex);
        if (np->nametype != NAMETYPE_UNDECLARED) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        lex = parser_next(pctx);
        if (lex->type != LEXTYPE_DELIM_LPAR &&
            lex->type != LEXTYPE_OP_ASSIGN &&
            (is_kwdmacro || lex->type == LEXTYPE_DELIM_LBRACK)) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        normp = condp = 0;
        body = 0;
        normcount = condcount = 0;
        if (lex->type == LEXTYPE_DELIM_LPAR) {
            if (!parse_paramlist(pctx, LEXTYPE_DELIM_RPAR, &normp, &normcount)) {
                skip_to_end = 1;
            }
            lexeme_free(lex);
            lex = parser_next(pctx);
        }
        if (lex->type == LEXTYPE_DELIM_LBRACK) {
            if (is_kwdmacro) {
                /* XXX error condition */
                skip_to_end = 1;
            }
            if (!parse_paramlist(pctx, LEXTYPE_DELIM_RBRACK, &condp, &condcount)) {
                skip_to_end = 1;
            }
            lexeme_free(lex);
            lex = parser_next(pctx);
        }
        if (lex->type == LEXTYPE_OP_ASSIGN) {
            lexeme_t *last;
            parser_set_quotelevel(pctx, QL_MACRO);
            parser_incr_erroneof(pctx);
            lexeme_free(lex);
            for (lex = parser_next(pctx); lex->type != LEXTYPE_DELIM_PERCENT;
                 lex = parser_next(pctx)) {
                if (lex->type == LEXTYPE_END || lex->type == LEXTYPE_NONE) {
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
                macro->kwdmacro = is_kwdmacro;
                macro->body = body;
                macro->paramcount = normcount;
                macro->paramlist = normp;
                macro->condparamcount = condcount;
                macro->condparams = condp;
                np->nametype = NAMETYPE_MACRO;
                np->namedata.ptr = macro;
            }
        } else {
            lexseq_free(body);
            free_paramlist(condp);
            free_paramlist(normp);
        }

        lex = parser_next(pctx);
        if (lex->type == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lex->type != LEXTYPE_DELIM_COMMA) {
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
