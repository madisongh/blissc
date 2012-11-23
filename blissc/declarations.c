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
#include "storage.h"
#include "strings.h"
#include "utils.h"

struct structdef_s {
    scopectx_t          accformals;
    scopectx_t          alloformals;
    lexseq_t            sizeexp;
    lexseq_t            body;
};
typedef struct structdef_s structdef_t;

struct fielddef_s {
    name_t          *struc;
    unsigned int    offset;
    unsigned int    pos;
    unsigned int    size;
    unsigned int    extend;
};
typedef struct fielddef_s fielddef_t;

static name_t decl_names[] = {
    NAMEDEF("LITERAL", LEXTYPE_DCL_LITERAL, NAME_M_RESERVED),
    NAMEDEF("GLOBAL", LEXTYPE_DCL_GLOBAL, NAME_M_RESERVED),
    NAMEDEF("COMPILETIME", LEXTYPE_DCL_COMPILETIME, NAME_M_RESERVED),
    NAMEDEF("MODULE", LEXTYPE_DCL_MODULE, NAME_M_RESERVED),
    NAMEDEF("ELUDOM", LEXTYPE_DCL_ELUDOM, NAME_M_RESERVED),
    NAMEDEF("ROUTINE", LEXTYPE_DCL_ROUTINE, NAME_M_RESERVED),
    NAMEDEF("LABEL", LEXTYPE_DCL_LABEL, NAME_M_RESERVED),
    NAMEDEF("LOCAL", LEXTYPE_DCL_LOCAL, NAME_M_RESERVED),
    NAMEDEF("OWN", LEXTYPE_DCL_OWN, NAME_M_RESERVED),
    NAMEDEF("STACKLOCAL", LEXTYPE_DCL_STACKLOCAL, NAME_M_RESERVED),
    NAMEDEF("EXTERNAL", LEXTYPE_DCL_EXTERNAL, NAME_M_RESERVED),
    NAMEDEF("STRUCTURE", LEXTYPE_DCL_STRUCTURE, NAME_M_RESERVED),
    NAMEDEF("LINKAGE", LEXTYPE_DCL_LINKAGE, NAME_M_RESERVED),
    NAMEDEF("BIND", LEXTYPE_DCL_BIND, NAME_M_RESERVED),
    NAMEDEF("PSECT", LEXTYPE_DCL_PSECT, NAME_M_RESERVED),
    NAMEDEF("BUILTIN", LEXTYPE_DCL_BUILTIN, NAME_M_RESERVED),
    NAMEDEF("UNDECLARE", LEXTYPE_DCL_UNDECLARE, NAME_M_RESERVED),
    NAMEDEF("SIGNED", LEXTYPE_ATTR_SIGNED, NAME_M_RESERVED),
    NAMEDEF("UNSIGNED", LEXTYPE_ATTR_UNSIGNED, NAME_M_RESERVED),
    NAMEDEF("BYTE", LEXTYPE_AU_BYTE, NAME_M_RESERVED),
    NAMEDEF("WORD", LEXTYPE_AU_WORD, NAME_M_RESERVED),
    NAMEDEF("LONG", LEXTYPE_AU_LONG, NAME_M_RESERVED),
    NAMEDEF("QUAD", LEXTYPE_AU_QUAD, NAME_M_RESERVED)
};

static int bind_compiletime(void *ctx, quotelevel_t ql, quotemodifier_t qm,
                            lextype_t lt, condstate_t cs, lexeme_t *lex,
                            lexseq_t *result);
static int declare_compiletime(parse_ctx_t pctx, scopectx_t scope,
                               lextype_t lt);

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

            if (!parse_ctce(pctx, &lex)) {
                /* XXX error condition */
                lexeme_free(nlex);
                return 0;
            } else {
                str = lexeme_text(nlex);
                val = lexeme_signedval(lex);
                np = name_declare(scope, str->ptr, str->len,
                                  LEXTYPE_NAME_COMPILETIME,
                                  lexeme_textpos_get(nlex),
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
 * bind_literal
 *
 * Binds a literal to its value.
 */
static int
bind_literal (void *ctx, quotelevel_t ql, quotemodifier_t qm,
                  lextype_t lt, condstate_t cs, lexeme_t *lex,
                  lexseq_t *result) {
    name_t *np = lexeme_ctx_get(lex);
    seg_t *seg = *(seg_t **)name_data(np);
    long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    if (!(seg->flags & SEG_M_HASVAL)) {
        lex->type = lex->boundtype = LEXTYPE_SEGMENT;
        return 0;
    }
    if (seg->flags & SEG_M_SIGNED) {
        val = (long)(seg->offset);
    } else {
        val = (unsigned long)(seg->offset);
    }
    string_free(&lex->text);
    string_printf(&lex->text, "%ld", val);
    lex->type = lex->boundtype = LEXTYPE_NUMERIC;
    lexeme_val_setsigned(lex, val);

    return 0;

} /* bind_literal */

/*
 * declare_literal
 *
 * Common logic for literal declarations.
 *
 * LITERAL name = value { : attribute } {,...}
 * GLOBAL LITERAL name = value { : attribute } {,...}
 * EXTERNAL LITERAL name { : attribute } {,...}
 */
static int
declare_literal (parse_ctx_t pctx, scopectx_t scope, stgctx_t stg,
                 machinedef_t *mach, int is_global, int is_external)
{
    lexeme_t *lex, *nlex;
    lextype_t lt;
    name_t *np;
    strdesc_t *str;
    long val;
    long range = mach->bpval;
    int is_signed = 1;
    seg_t *litseg;


    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &nlex, 0)) {
            /* XXX error condition */
            return 0;
        }
        if (!is_external) {
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
                /* XXX error condition */
                lexeme_free(nlex);
                return 0;
            } else {
                if (!parse_ctce(pctx, &lex)) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                } else {
                    val = lexeme_signedval(lex);
                    lexeme_free(lex);
                }
            }

        }
        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_COLON) {
            lt = parser_next(pctx, QL_NORMAL, 0);
            if (lt == LEXTYPE_ATTR_SIGNED || lt == LEXTYPE_ATTR_UNSIGNED) {
                is_signed = lt == LEXTYPE_ATTR_SIGNED;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                }
                if (!parse_ctce(pctx, &lex)) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                }
                range = lexeme_signedval(lex);
                lexeme_free(lex);
                if (range < 1 || range > mach->bpval) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                }
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                    /* XXX error condition */
                    lexeme_free(nlex);
                    return 0;
                }
            }
            // now get the semicolon or comma, for later
            lt = parser_next(pctx, QL_NORMAL, 0);
        }
        if (bits_needed(labs(val)) > mach->bpval - is_signed) {
            /* XXX error condition */
        }
        str = lexeme_text(nlex);
        litseg = seg_alloc(stg, lexeme_textpos_get(nlex));
        litseg->type = SEGTYPE_LITERAL;
        litseg->flags = (is_external ? 0 : SEG_M_HASVAL) |
                       (is_signed ? SEG_M_SIGNED : 0) |
                       (is_global ? SEG_M_GLOBAL : 0);
        litseg->size = range;
        if (is_signed) {
            litseg->offset = labs(val) & ((1UL<<range)-1);
            if (val < 0) {
                litseg->offset = -litseg->offset;
            }
        } else {
            litseg->offset = val & ((1UL<<range)-1);
        }
        np = name_declare(scope, str->ptr, str->len,
                          LEXTYPE_NAME_LITERAL,
                          lexeme_textpos_get(nlex),
                          &litseg, sizeof(litseg));
        lexeme_free(nlex);
        if (np == 0) {
            /* XXX error condition */
        }

        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            return 0;
        }
        
    } /* while 1 */

    return 1;

} /* declare_literal */

/*
 * declare_label
 *
 * Declare a label.
 *
 * LABEL name {,...}
 */
static int
declare_label (parse_ctx_t pctx, scopectx_t scope)
{
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    strdesc_t *str;


    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &lex, 0)) {
            /* XXX error condition */
            return 0;
        }
        str = lexeme_text(lex);
        np = name_declare(scope, str->ptr, str->len,
                          LEXTYPE_NAME_LABEL,
                          lexeme_textpos_get(lex),
                          0, 0);
        lexeme_free(lex);
        if (np == 0) {
            /* XXX error condition */
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition - maybe just forgot? */
            parser_insert(pctx, lex);
        }
    }
    lexeme_free(lex);
    return 1;

} /* declare_label */


seg_t *
define_plit (parse_ctx_t pctx, stgctx_t stg, lextype_t curlt)
{
    // Handle the parsing of a plit definition
    return 0;
}

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
    lextype_register(LEXTYPE_NAME_LITERAL, bind_literal);
    macros_init(kwdscope);

} /* declarations_init */

/*
 * parse_declaration
 */
int
parse_declaration (parse_ctx_t pctx, lextype_t lt)
{
    scopectx_t scope = parser_scope_get(pctx);
    lexeme_t *lex;
    int is_global = 0, is_external = 0;
    int status;
    stgctx_t stg = parser_get_cctx(pctx);
    machinedef_t *mach = parser_get_machinedef(pctx);

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
        case LEXTYPE_DCL_LITERAL:
            status = declare_literal(pctx, scope, stg, mach,
                                     is_global, is_external);
            break;
        case LEXTYPE_DCL_LABEL:
            status = declare_label(pctx, scope);
        default:
            /* XXX error condition */
            status = 0;
            break;
    }

    return status;

} /* parse_declaration */

/*
 * parse_module
 */
int
declare_module (parse_ctx_t pctx)
{
    scopectx_t scope = parser_scope_get(pctx);
    lextype_t lt;
    lexeme_t *lex;
    strdesc_t *text;
    expr_node_t *blkexp;
    name_t *np;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_MODULE, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    lt = parser_next(pctx, QL_NAME, &lex);
    if (lt != LEXTYPE_NAME) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    text = lexeme_text(lex);
    np = name_declare(scope, text->ptr, text->len,
                      LEXTYPE_NAME_MODULE,
                      lexeme_textpos_get(lex),
                      0, 0);
    lexeme_free(lex);
    if (np == 0) {
        /* XXX error condition */
        return 0;
    }

    // XXX need to handle module-switches here
    
    if (!expr_parse_block(pctx, &blkexp)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_ELUDOM, 0, 0)) {
        /* XXX error condition */
        // but ignore the error
    }

    *(expr_node_t **)name_data(np) = blkexp;

    return 1;

} /* parse_module */