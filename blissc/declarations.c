//
//  declarations.c
//  blissc
//
//  Created by Matthew Madison on 11/11/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "parser.h"
#include "lexer.h"
#include "nametable.h"
#include "lexeme.h"
#include "expression.h"
#include "strings.h"

struct literal_s {
    int something;
};

static int parse_LITERAL(parse_ctx_t pctx);
static int parse_GLOBAL(parse_ctx_t pctx);

static name_t decl_names[] = {
    NAMEDEF("LITERAL", LEXTYPE_DCL_LITERAL, NAME_M_RESERVED),
    NAMEDEF("GLOBAL", LEXTYPE_DCL_GLOBAL, NAME_M_RESERVED)
};

/*
 * declarations_init
 *
 * Initialization routine.
 */
void
declarations_init (scopectx_t kwdscope)
{
    int i;
    for (i = 0; i < sizeof(decl_names)/sizeof(decl_names[0]); i++) {
        name_insert(kwdscope, &decl_names[i]);
    }

} /* declarations_init */

/*
 * define_literal
 *
 * Common logic for literal declarations.
 *
 * LITERAL name = value { : attribute } {,...}
 * GLOBAL LITERAL name = value { : attribute } {,...}
 */
static int
define_literal (parse_ctx_t pctx, int is_global)
{
    int skip_to_end = 0;
    quotelevel_t oldql;
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    scopectx_t scope;

    if (!parser_decl_ok(pctx, &scope)) {
        /* XXX error condition */
        skip_to_end = 1;
    }

    oldql = parser_set_quotelevel(pctx, QL_NAME);

    while (1) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_NAME) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        np = name_search(scope, lex->text.ptr, lex->text.len, 0);
        lexeme_free(lex);
        if (name_flags(np) & NAME_M_RESERVED) {
            /* XXX error condition */
            skip_to_end = 1;
        }
        if (name_type(np) != LEXTYPE_NAME && name_scope(np) == scope) {
            /* XXX redeclaration error */
            skip_to_end = 1;
        }
        lt = parser_next(pctx, &lex);
        lexeme_free(lex);
        if (lt != LEXTYPE_OP_ASSIGN) {
            /* XXX error condition */
            skip_to_end = 1;
        } else {
            parser_set_quotelevel(pctx, QL_NORMAL);

            if (!parse_Expression(pctx)) {
                /* XXX error condition */
                skip_to_end = 1;
            } else {
                lt = parser_next(pctx, &lex);
                // process the value
                lt = parser_next(pctx, &lex);
                if (lt == LEXTYPE_DELIM_COLON) {
                    lexeme_free(lex);
                    // process the attribute(s)
                    lt = parser_next(pctx, &lex);
                }
            }

            parser_set_quotelevel(pctx, QL_NAME);

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

    parser_set_quotelevel(pctx, oldql);
    lexeme_free(lex);
    return 1;

} /* define_literal */

static int parse_LITERAL (parse_ctx_t pctx) { return define_literal(pctx, 0); }

static int
parse_GLOBAL (parse_ctx_t pctx)
{
    return 1;
}
