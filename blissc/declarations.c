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
    quotelevel_t oldql;
    lexeme_t *lex;
    int is_global = 0, is_external = 0;
    int status;

    if (!parser_decl_ok(pctx, &scope)) {
        /* XXX error condition */
        return 1;
    }

    oldql = parser_set_quotelevel(pctx, QL_NAME);
    if (lt == LEXTYPE_DCL_GLOBAL) {
        is_global = 1;
        lt = parser_next(pctx, &lex);
        if (lt == LEXTYPE_NAME) {
            // status = declare_global(pctx, lex);
        } else if (!(lt == LEXTYPE_DCL_LITERAL ||
                     lt == LEXTYPE_DCL_ROUTINE)) {
            /* XXX error condition */
            lexeme_free(lex);
            parser_set_quotelevel(pctx, oldql);
            return 1;
        }
        lexeme_free(lex);
    }
    if (!is_global && lt == LEXTYPE_DCL_EXTERNAL) {
        is_external = 1;
        lt = parser_next(pctx, &lex);
        if (!(lt == LEXTYPE_DCL_LITERAL ||
              lt == LEXTYPE_DCL_ROUTINE)) {
            /* XXX error condition */
            lexeme_free(lex);
            parser_set_quotelevel(pctx, oldql);
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

    parser_set_quotelevel(pctx, oldql);
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
    lexeme_t *rlex;
    strdesc_t *valdsc;
    unsigned long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    val = *(long *)name_data(np);
    valdsc = string_printf(0, "%ld", val);
    rlex = lexeme_create(LEXTYPE_NUMERIC, valdsc);
    lexeme_val_setunsigned(rlex, val);
    string_free(valdsc);
    lexeme_free(lex);
    lexseq_instail(result, rlex);
    return 1;
    
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
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;

    while (1) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_NAME) {
            /* XXX error condition */
            lexeme_free(lex);
            return 0;
        }
        np = lexeme_ctx_get(lex);
        lexeme_free(lex);
        if (name_flags(np) & NAME_M_RESERVED) {
            /* XXX error condition */
            return 0;
        }
        if (name_type(np) != LEXTYPE_NAME && name_scope(np) == scope) {
            /* XXX redeclaration error */
            return 0;
        }
        lt = parser_next(pctx, &lex);
        lexeme_free(lex);
        if (lt != LEXTYPE_OP_ASSIGN) {
            /* XXX error condition */
            return 0;
        } else {
            parser_set_quotelevel(pctx, QL_NORMAL);

            if (!parse_Expression(pctx)) {
                /* XXX error condition */
                return 0;
            } else {
                lt = parser_next(pctx, &lex);
                if (lt != LEXTYPE_NUMERIC) {
                    /* XXX error condition */
                    lexeme_free(lex);
                    return 0;
                }
                np->nametype = LEXTYPE_NAME_COMPILETIME;
                *(long *)name_data(np) = lexeme_signedval(lex);
                lexeme_free(lex);
            }

        }

        lt = parser_next(pctx, &lex);
        lexeme_free(lex);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            return 0;
        }

        parser_set_quotelevel(pctx, QL_NAME);

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
