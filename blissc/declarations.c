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
#include "macros.h"
#include "strings.h"

struct literal_s {
    int something;
};

static int parse_LITERAL(parse_ctx_t pctx);
static int parse_GLOBAL(parse_ctx_t pctx);

static name_t decl_names[] = {
    NAMEDEF("LITERAL", LEXTYPE_DCL_LITERAL, NAME_M_RESERVED),
    NAMEDEF("GLOBAL", LEXTYPE_DCL_GLOBAL, NAME_M_RESERVED),
    NAMEDEF("COMPILETIME", LEXTYPE_DCL_COMPILETIME, NAME_M_RESERVED)
};

static int bind_compiletime(void *ctx, quotelevel_t ql, quotemodifier_t qm,
                            lextype_t lt, condstate_t cs, lexeme_t *lex,
                            lexseq_t *result);
static int declare_compiletime(parse_ctx_t pctx, scopectx_t scope,
                               lextype_t lt);

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

    lextype_register(LEXTYPE_NAME_COMPILETIME, bind_compiletime);

    macros_init(kwdscope);

} /* declarations_init */

/*
 * parse_declaration
 */
int
parse_declaration (parse_ctx_t pctx, lextype_t lt)
{
    scopectx_t scope;
    lexeme_t *lex;
    int is_global = 0, is_external = 0;
    int status;

    if (!parser_decl_ok(pctx, &scope)) {
        /* XXX error condition */
        return 1;
    }

    if (lt == LEXTYPE_DCL_GLOBAL) {
        is_global = 1;
        lt = parser_next(pctx, QL_NAME, &lex);
        if (lt == LEXTYPE_NAME) {
            // status = declare_global(pctx, lex);
        } else if (!(lt == LEXTYPE_DCL_LITERAL ||
                     lt == LEXTYPE_DCL_ROUTINE)) {
            /* XXX error condition */
            lexeme_free(lex);
            return 1;
        }
        lexeme_free(lex);
    }
    if (!is_global && lt == LEXTYPE_DCL_EXTERNAL) {
        is_external = 1;
        lt = parser_next(pctx, QL_NAME, &lex);
        if (!(lt == LEXTYPE_DCL_LITERAL ||
              lt == LEXTYPE_DCL_ROUTINE)) {
            /* XXX error condition */
            lexeme_free(lex);
            return 1;
        }
        lexeme_free(lex);
    }

    switch (lt) {
        case LEXTYPE_DCL_MACRO:
        case LEXTYPE_DCL_KEYWORDMACRO:
            status = declare_macro(pctx, scope, lt);
            break;
        case LEXTYPE_DCL_COMPILETIME:
            status = declare_compiletime(pctx, scope, lt);
            break;
        default:
            /* XXX error condition */
            status = 0;
            break;
    }

    return status;
    
} /* parse_declaration */

/*
 * bind_compiletime
 *
 * Binds a COMPILETIME name to its value.
 */
static int
bind_compiletime (void *ctx, quotelevel_t ql, quotemodifier_t qm,
                  lextype_t lt, condstate_t cs, lexeme_t *lex,
                  lexseq_t *result) {
    name_t *np = lexeme_ctx_get(lex);
    long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    val = *(long *)name_data(np);
    string_free(&lex->text);
    string_printf(&lex->text, "%ld", val);
    lex->type = lex->boundtype = LEXTYPE_NUMERIC;
    lexeme_val_setsigned(lex, val);

    return 0;
    
} /* bind_compiletime */

/*
 * declare_compiletime
 *
 * Define COMPILETIME names.
 *
 * COMPILETIME name = value {,...}
 */
static int
declare_compiletime (parse_ctx_t pctx, scopectx_t scope, lextype_t curlt)
{
    lexeme_t *lex, *nlex;
    lextype_t lt;
    name_t *np;
    strdesc_t *str;
    long val;

    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &nlex, 0)) {
            /* XXX error condition */
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
            /* XXX error condition */
            lexeme_free(nlex);
            return 0;
        } else {

            if (!parse_Expression(pctx)) {
                /* XXX error condition */
                lexeme_free(nlex);
                return 0;
            } else {
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NUMERIC, &lex, 0)) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                }
                str = lexeme_text(nlex);
                val = lexeme_signedval(lex);
                np = name_declare(scope, str->ptr, str->len,
                                  LEXTYPE_NAME_COMPILETIME,
                                  &val, sizeof(val));
                if (np == 0) {
                    /* XXX error condition */
                }
                lexeme_free(lex);
                lexeme_free(nlex);
            }

        }

        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            return 0;
        }

    } /* while 1 */

    return 1;
    
} /* declare_compiletime */

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
    
    return 1;

} /* define_literal */
