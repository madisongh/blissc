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
#include "declarations.h"
#include "structures.h"
#include "macros.h"
#include "storage.h"
#include "strings.h"
#include "utils.h"

typedef enum {
    DCL_NORMAL = 0,
    DCL_GLOBAL,
    DCL_EXTERNAL,
    DCL_FORWARD,
    DCL_MAP
} decltype_t;


#define ALLOC_QTY 128

struct declctx_s {
    scopectx_t  globals;
    symbol_t    *freenis;
};
typedef struct declctx_s *declctx_t;

static name_t decl_names[] = {
    NAMEDEF("LITERAL", LEXTYPE_DCL_LITERAL, NAME_M_RESERVED),
    NAMEDEF("GLOBAL", LEXTYPE_DCL_GLOBAL, NAME_M_RESERVED),
    NAMEDEF("EXTERNAL", LEXTYPE_DCL_EXTERNAL, NAME_M_RESERVED),
    NAMEDEF("FORWARD", LEXTYPE_DCL_FORWARD, NAME_M_RESERVED),
    NAMEDEF("COMPILETIME", LEXTYPE_DCL_COMPILETIME, NAME_M_RESERVED),
    NAMEDEF("MAP", LEXTYPE_DCL_MAP, NAME_M_RESERVED),
    NAMEDEF("MODULE", LEXTYPE_DCL_MODULE, NAME_M_RESERVED),
    NAMEDEF("ELUDOM", LEXTYPE_DCL_ELUDOM, NAME_M_RESERVED),
    NAMEDEF("ROUTINE", LEXTYPE_DCL_ROUTINE, NAME_M_RESERVED),
    NAMEDEF("LABEL", LEXTYPE_DCL_LABEL, NAME_M_RESERVED),
    NAMEDEF("LOCAL", LEXTYPE_DCL_LOCAL, NAME_M_RESERVED),
    NAMEDEF("OWN", LEXTYPE_DCL_OWN, NAME_M_RESERVED),
    NAMEDEF("STACKLOCAL", LEXTYPE_DCL_STACKLOCAL, NAME_M_RESERVED),
    NAMEDEF("STRUCTURE", LEXTYPE_DCL_STRUCTURE, NAME_M_RESERVED),
    NAMEDEF("FIELD", LEXTYPE_DCL_FIELD, NAME_M_RESERVED),
    NAMEDEF("LINKAGE", LEXTYPE_DCL_LINKAGE, NAME_M_RESERVED),
    NAMEDEF("BIND", LEXTYPE_DCL_BIND, NAME_M_RESERVED),
    NAMEDEF("PSECT", LEXTYPE_DCL_PSECT, NAME_M_RESERVED),
    NAMEDEF("BUILTIN", LEXTYPE_DCL_BUILTIN, NAME_M_RESERVED),
    NAMEDEF("UNDECLARE", LEXTYPE_DCL_UNDECLARE, NAME_M_RESERVED),
    NAMEDEF("REQUIRE", LEXTYPE_DCL_REQUIRE, NAME_M_RESERVED),
    NAMEDEF("SIGNED", LEXTYPE_ATTR_SIGNED, NAME_M_RESERVED),
    NAMEDEF("UNSIGNED", LEXTYPE_ATTR_UNSIGNED, NAME_M_RESERVED),
    NAMEDEF("VOLATILE", LEXTYPE_ATTR_VOLATILE, NAME_M_RESERVED),
    NAMEDEF("ALIAS", LEXTYPE_ATTR_ALIAS, NAME_M_RESERVED),
    NAMEDEF("INITIAL", LEXTYPE_ATTR_INITIAL, NAME_M_RESERVED),
    NAMEDEF("PRESET", LEXTYPE_ATTR_PRESET, NAME_M_RESERVED),
    NAMEDEF("ALIGN", LEXTYPE_ATTR_ALIGN, NAME_M_RESERVED),
    NAMEDEF("BYTE", LEXTYPE_AU_BYTE, NAME_M_RESERVED),
    NAMEDEF("WORD", LEXTYPE_AU_WORD, NAME_M_RESERVED),
    NAMEDEF("LONG", LEXTYPE_AU_LONG, NAME_M_RESERVED),
    NAMEDEF("QUAD", LEXTYPE_AU_QUAD, NAME_M_RESERVED),
    NAMEDEF("CODE", LEXTYPE_KWD_CODE, NAME_M_RESERVED),
    NAMEDEF("NODEFAULT", LEXTYPE_KWD_NODEFAULT, NAME_M_RESERVED),
    NAMEDEF("WRITE", LEXTYPE_KWD_WRITE, 0),
    NAMEDEF("NOWRITE", LEXTYPE_KWD_NOWRITE, 0),
    NAMEDEF("EXECUTE", LEXTYPE_KWD_EXECUTE, 0),
    NAMEDEF("NOEXECUTE", LEXTYPE_KWD_NOEXECUTE, 0),
    NAMEDEF("OVERLAY", LEXTYPE_KWD_OVERLAY, 0),
    NAMEDEF("CONCATENATE", LEXTYPE_KWD_CONCATENATE, 0),
    NAMEDEF("REF", LEXTYPE_KWD_REF, 0),
    NAMEDEF("NOVALUE", LEXTYPE_ATTR_NOVALUE, NAME_M_RESERVED)
};

static int bind_compiletime(void *ctx, quotelevel_t ql, quotemodifier_t qm,
                            lextype_t lt, condstate_t cs, lexeme_t *lex,
                            lexseq_t *result);
static int declare_compiletime(expr_ctx_t ctx, scopectx_t scope, lextype_t lt);

symbol_t *
symbol_alloc (expr_ctx_t ctx, symtype_t type)
{
    declctx_t dctx = expr_decl_ctx(ctx);

    symbol_t *ni;
    int i;
    if (dctx->freenis == 0) {
        dctx->freenis = malloc(ALLOC_QTY*sizeof(symbol_t));
        if (dctx->freenis == 0) {
            return 0;
        }
        for (i = 0, ni = dctx->freenis; i < ALLOC_QTY-1; i++, ni++) {
            symbol_next_set(ni, ni+1);
        }
        symbol_next_set(ni, 0);
    }
    ni = dctx->freenis;
    dctx->freenis = symbol_next(ni);
    memset(ni, 0, sizeof(symbol_t));
    symbol_type_set(ni, type);
    return ni;

} /* symbol_alloc */

void
symbol_free (expr_ctx_t ctx, symbol_t *ni)
{
    declctx_t dctx = expr_decl_ctx(ctx);

    if (ni == 0) {
        return;
    }
    if (symbol_type(ni) == SYMTYPE_ROUTINE) {
        expr_node_free(ctx, symbol_routine_expr(ni));
    }
    memset(ni, 0x42, sizeof(symbol_t));
    symbol_next_set(ni, dctx->freenis);
    dctx->freenis = ni;

} /* symbol_free */

symbol_t *
symbol_copy (expr_ctx_t ctx, symbol_t *src)
{
    symbol_t *dst;

    if (src == 0) {
        return 0;
    }
    dst = symbol_alloc(ctx, sizeof(symbol_t));
    if (dst == 0) {
        return 0;
    }
    memcpy(dst, src, sizeof(symbol_t));

    return dst;
}

/*
 * bind_compiletime
 *
 * Binds a COMPILETIME name to its value.
 * This is a lexical binding function, invoked through
 * lexeme_bind.
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
    val = name_data_int(np);
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
declare_compiletime (expr_ctx_t ctx, scopectx_t scope, lextype_t curlt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
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

            if (!expr_parse_ctce(ctx, &lex)) {
                /* XXX error condition */
                lexeme_free(nlex);
                return 0;
            } else {
                str = lexeme_text(nlex);
                val = lexeme_signedval(lex);
                np = name_declare(scope, str->ptr, str->len,
                                  LEXTYPE_NAME_COMPILETIME,
                                  lexeme_textpos_get(nlex));
                name_data_set_int(np, val);
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
    symbol_t *ni = name_data_ptr(np);
    symtype_t nt = symbol_type(ni);
    long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    if (nt != SYMTYPE_LITERAL) {
        /* XXX something is very wrong */
        return -1;
    }
    val = getvalue(symbol_lit_val(ni),
                   symbol_lit_valwidth(ni),
                   symbol_lit_signextend(ni));
    lexeme_type_set(lex, LEXTYPE_NUMERIC);
    string_free(lexeme_text(lex));
    string_printf(lexeme_text(lex), "%ld", val);
    lexeme_val_setsigned(lex, val);
    return 0;

} /* bind_literal */

/*
 * parse_decl_name
 *
 * Common parsing logic for names to be declared.
 */
int
parse_decl_name (parse_ctx_t pctx, scopectx_t scope,
                 strdesc_t **result, textpos_t *pos)
{
    lextype_t lt;
    lexeme_t *lex;

    lt = parser_next(pctx, QL_NAME, &lex);

    if (lt == LEXTYPE_NAME) {
        *result = string_copy(0, lexeme_text(lex));
        *pos = lexeme_textpos_get(lex);
        lexeme_free(lex);
        return 1;
    }

    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        name_t *np = lexeme_ctx_get(lex);
        if (!(name_flags(np) & NAME_M_RESERVED) &&
            name_scope(np) != scope) {
                *result = string_copy(0, lexeme_text(lex));
                *pos = lexeme_textpos_get(lex);
                lexeme_free(lex);
                return 1;
        }
        /* XXX error condition - redeclaration */
        lexeme_free(lex);
        return 0;
    }
    /* XXX error condition - expected name */
    parser_insert(pctx, lex);
    return 0;

} /* parse_decl_name */

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
declare_literal (expr_ctx_t ctx, scopectx_t scope, decltype_t decltype)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    declctx_t dctx = expr_decl_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    textpos_t pos;
    strdesc_t *namestr;
    long val;
    unsigned int range = machine_scalar_bits(mach);
    int hasvalue, is_signed;
    symbol_t *ni, *extsym;


    while (1) {
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        if (decltype != DCL_EXTERNAL) {
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
                /* XXX error condition */
                string_free(namestr);
                return 0;
            } else {
                if (!expr_parse_ctce(ctx, &lex)) {
                    /* XXX error condition */
                    string_free(namestr);
                    return 0;
                } else {
                    val = lexeme_signedval(lex);
                    lexeme_free(lex);
                }
            }

        }
        is_signed = 1;
        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_COLON) {
            lt = parser_next(pctx, QL_NORMAL, 0);
            if (lt == LEXTYPE_ATTR_SIGNED || lt == LEXTYPE_ATTR_UNSIGNED) {
                long rval;
                is_signed = lt == LEXTYPE_ATTR_SIGNED;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    /* XXX error condition */
                }
                if (!expr_parse_ctce(ctx, &lex)) {
                    /* XXX error condition */
                    rval = machine_scalar_bits(mach);
                } else {
                    rval = lexeme_signedval(lex);
                    lexeme_free(lex);
                }
                if (!machine_signext_supported(mach)) {
                    /* XXX error condition */
                } else if (rval < 1 || rval > machine_scalar_bits(mach)) {
                    /* XXX error condition */
                } else {
                    range = (unsigned int) rval;
                }
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                    /* XXX error condition */
                    string_free(namestr);
                    return 0;
                }
            }
            // now get the semicolon or comma, for later
            lt = parser_next(pctx, QL_NORMAL, 0);
        }
        if (decltype != DCL_EXTERNAL &&
            bits_needed(labs(val)) > machine_scalar_bits(mach) - is_signed) {
            /* XXX error condition */
        }
        extsym = 0;
        hasvalue = 0;
        if (decltype == DCL_GLOBAL || decltype == DCL_EXTERNAL) {
            name_t *extnp = name_search(dctx->globals, namestr->ptr,
                                        namestr->len, 1);
            if (extnp == 0) {
                /* XXX error condition */
                return 0;
            }
            if (name_type(extnp) == LEXTYPE_NAME) {
                extsym = symbol_alloc(ctx, SYMTYPE_LITERAL);
                if (decltype == DCL_GLOBAL) {
                    symbol_lit_val_set(extsym, val);
                    symbol_lit_hasvalue_set(extsym, 1);
                    symbol_lit_valwidth_set(extsym, range);
                    symbol_lit_signextend_set(extsym, is_signed);
                }
                extnp = name_declare(dctx->globals, namestr->ptr,
                             namestr->len, LEXTYPE_NAME_DATA, pos);
                name_data_set_ptr(extnp, extsym);
            } else {
                if (decltype == DCL_GLOBAL) {
                    /* XXX error - redeclaration */
                }
                extsym = name_data_ptr(extnp);
                if (symbol_type(extsym) != SYMTYPE_LITERAL) {
                    /* XXX error condition */
                    return 0;
                }
                if (decltype == DCL_EXTERNAL) {
                    if (symbol_lit_valwidth(extsym) != range ||
                        symbol_lit_signextend(extsym) != is_signed) {
                        /* XXX attribute mismatch */
                    }
                    if (symbol_lit_hasvalue(extsym)) {
                        val = getvalue(symbol_lit_val(extsym), symbol_lit_valwidth(extsym), symbol_lit_signextend(extsym));
                        hasvalue = 1;
                    }
                }
            }
        } /* check global symbol table */
        ni = symbol_alloc(ctx, SYMTYPE_LITERAL);
        if (decltype != DCL_EXTERNAL || hasvalue) {
            symbol_lit_val_set(ni, val);
            symbol_lit_hasvalue_set(ni, 1);
        }
        symbol_lit_valwidth_set(ni, range);
        symbol_lit_signextend_set(ni, is_signed);

        np = name_declare(scope, namestr->ptr, namestr->len,
                          LEXTYPE_NAME_LITERAL, pos);
        name_data_set_ptr(np, ni);
        string_free(namestr);
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
    name_t *np;
    strdesc_t *namestr;
    textpos_t pos;


    while (1) {
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        np = name_declare(scope, namestr->ptr, namestr->len,
                          LEXTYPE_NAME_LABEL, pos);
        if (np == 0) {
            /* XXX error condition */
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition - maybe just forgot? */
        }
    }
    return 1;

} /* declare_label */

/*
 * declare_psect
 *
 * Declare a PSECT.
 *
 * PSECT OWN|GLOBAL|PLIT|CODE|NODEFAULT = name { (attribute,...) }
 */
static int
psect_attr (parse_ctx_t pctx, unsigned int *attrp)
{
    unsigned int attr = 0;
    int which;
    static lextype_t akwds[] = { LEXTYPE_KWD_WRITE, LEXTYPE_KWD_NOWRITE,
        LEXTYPE_KWD_EXECUTE, LEXTYPE_KWD_NOEXECUTE, LEXTYPE_KWD_OVERLAY,
        LEXTYPE_KWD_CONCATENATE };
    static unsigned int values[] = { PSECT_M_ATTR_WRITE, 0, PSECT_M_ATTR_EXEC, 0,
        PSECT_M_ATTR_OVERLAY, 0 };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        return 0;
    }
    while (1) {
        which = parser_expect_oneof(pctx, QL_NORMAL,
                                 akwds, sizeof(akwds)/sizeof(akwds[0]), 0, 1);
        if (which < 0) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return -1;
        }
        attr |= values[which];
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
        }
    }

    *attrp = attr;
    return 1;
}

static int
declare_psect (expr_ctx_t ctx, scopectx_t scope)
{
    stgctx_t stg = expr_stg_ctx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    name_t *np;
    int which, n, has_attr;
    unsigned int attr;
    lexeme_t *lex;
    textpos_t defpos;
    static lextype_t classkws[] = { LEXTYPE_KWD_NODEFAULT,
        LEXTYPE_DCL_OWN, LEXTYPE_DCL_GLOBAL,
        LEXTYPE_KWD_PLIT, LEXTYPE_KWD_CODE };
    static storageclass_t classes[] = { 0, SCLASS_OWN, SCLASS_GLOBAL,
        SCLASS_PLIT, SCLASS_CODE };
    static lextype_t ntypes[] = { LEXTYPE_NAME, LEXTYPE_NAME_PSECT };


    while (1) {
        which = parser_expect_oneof(pctx, QL_NORMAL, classkws,
                                 sizeof(classkws)/sizeof(classkws[0]), 0, 1);
        if (which < 0) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            /* XXX error condition */
        }
        n = parser_expect_oneof(pctx, QL_NAME, ntypes, 2, &lex, 1);
        if (n < 0) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        defpos = lexeme_textpos_get(lex);
        has_attr = psect_attr(pctx, &attr);
        if (has_attr < 0) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        if (n == 1) {
            np = lexeme_ctx_get(lex);
            if (has_attr) {
                // check for attribute compatibility XXX
            }
        } else {
            strdesc_t *text = lexeme_text(lex);
            psect_t *ps;
            ps = psect_create(stg, lexeme_text(lex), defpos, attr);
            np = name_declare(scope, text->ptr, text->len,
                              LEXTYPE_NAME_PSECT, defpos);
            name_data_set_ptr(np, ps);
        }
        if (which > 0) {
            scope_sclass_psectname_set(scope, classes[which], np);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
            /* but continue? */
        }
    }

    return 1;

} /* declare_psect */

static int
name_data_value (name_t *np, seg_t **segp, expr_node_t **expp)
{
    symbol_t *ni;

    if (np == 0 || name_type(np) != LEXTYPE_NAME_DATA) {
        return 0;
    }
    ni = name_data_ptr(np);
    if (symbol_type(ni) == SYMTYPE_DATASEG) {
        *segp = symbol_data_seg(ni);
        return 0;
    }

    return -1;

} /* name_data_value */

static int
attr_psect (expr_ctx_t ctx, psect_t **psp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    psect_t *ps = 0;
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_PSECT, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    if (parser_expect(pctx, QL_NAME, LEXTYPE_NAME_PSECT, &lex, 1)) {
        ps = name_data_ptr(lexeme_ctx_get(lex));
        if (ps == 0) {
            /* XXX error condition */
        }
        lexeme_free(lex);
        *psp = ps;
    } else {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }

    return (ps != 0);

} /* attr_psect */

static int
attr_allocunit (expr_ctx_t ctx, int *valp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    int which;
    static lextype_t aus[] = { LEXTYPE_AU_BYTE, LEXTYPE_AU_WORD,
        LEXTYPE_AU_LONG, LEXTYPE_AU_QUAD };

    if (machine_unit_bits(mach) != 8) {
        return 0;  // weird word size, don't recognize the AUs
    }
    which = parser_expect_oneof(pctx, QL_NORMAL, aus, 4, 0, 1);
    if (which < 0) {
        return 0;
    }
    if (1<<which > machine_scalar_maxbytes(mach)) {
        /* XXX error condition */
        *valp = machine_scalar_maxbytes(mach);
    } else {
        *valp = 1 << which;
    }
    return 1;
}


/*
 * plit_items
 *
 * Parses an initval list for a PLIT, UPLIT, or the INITIAL
 * attribute on a data declaration.
 */
static initval_t *
plit_items (expr_ctx_t ctx, int defau) {

    stgctx_t stg = expr_stg_ctx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    initval_t *ivlist, *iv;
    int itemau = defau;
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    ivlist = 0;
    while (1) {
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REP, 0, 1)) {
            unsigned int repcount;
            if (!expr_parse_ctce(ctx, &lex)) {
                /* XXX error condition */
                repcount = 1;
            } else {
                repcount = (unsigned int)lexeme_unsignedval(lex);
                lexeme_free(lex);
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
                /* XXX error condition */
            }
            if (attr_allocunit(ctx, &itemau) == 0) {
                itemau = defau;
            }
            iv = plit_items(ctx, itemau);
            if (iv == 0) {
                /* XXX error condition */
            } else {
                ivlist = initval_ivlist_add(stg, ivlist, repcount, iv);
            }
        } else if (attr_allocunit(ctx, &itemau) != 0) {
            iv = plit_items(ctx, itemau);
            if (iv == 0) {
                /* XXX error condition */
            } else {
                ivlist = initval_ivlist_add(stg, ivlist, 1, iv);
            }
        } else {
            expr_node_t *exp;
            lex = 0;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
                ivlist = initval_string_add(stg, ivlist, 1, lexeme_text(lex));
                lexeme_free(lex);
            } else if (expr_expr_next(ctx, &exp)) {
                ivlist = expr_initval_add(ctx, ivlist, exp, itemau);
            } else {
                /* XXX error condition */
                initval_freelist(stg, ivlist);
                return 0;
            }
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
        }
    }
    return ivlist;
    
} /* plit_items */

/*
 * define_plit
 *
 * Allocates storage for a PLIT/UPLIT, and parses its initializers,
 * setting up the initval sequence.
 */
seg_t *
define_plit (expr_ctx_t ctx, lextype_t curlt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    int plitau;
    seg_t *seg;
    psect_t *psect;
    initval_t *ivlist;

    if (!attr_psect(ctx, &psect)) {
        name_t *np = scope_sclass_psectname(parser_scope_get(pctx), SCLASS_PLIT);
        psect = name_data_ptr(np);
    }
    if (!attr_allocunit(ctx, &plitau)) {
        plitau = machine_scalar_units(mach);
    }
    ivlist = plit_items(ctx, plitau);
    if (ivlist == 0) {
        return 0;
    }
    if (curlt == LEXTYPE_KWD_PLIT) {
        unsigned long size;
        unsigned int padding;
        size = initval_size(stg, ivlist);
        padding = size % machine_scalar_units(mach);
        // Must pad out to integral number of fullwords
        if (padding != 0) {
            ivlist = initval_scalar_add(stg, ivlist, padding, 0,
                                        machine_unit_bits(mach), 0);
        }
        size = initval_size(stg, ivlist) / machine_scalar_units(mach);
        ivlist = initval_scalar_prepend(stg, ivlist, 1, size,
                                        machine_scalar_bits(mach), 0);
    }

    seg = seg_alloc_static(stg, parser_curpos(pctx), psect);
    if (!seg_initval_set(stg, seg, ivlist)) {
        /* XXX error conditon */
        seg_free(stg, seg);
    } else {
        seg_commit(stg, seg);
    }

    return seg;
    
} /* define_plit */

static int
attr_extension (expr_ctx_t ctx, int *signext)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    int which;
    static lextype_t attrs[] = { LEXTYPE_ATTR_UNSIGNED,
        LEXTYPE_ATTR_SIGNED };

    if (!mach->signext_supported) {
        return -1;
    }
    which = parser_expect_oneof(pctx, QL_NORMAL, attrs,
                                sizeof(attrs)/sizeof(attrs[0]), 0, 1);
    if (which < 0) {
        return 0;
    }
    *signext = which;
    return 1;
}

static int
attr_align (expr_ctx_t ctx, int *valp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    lexeme_t *lex;
    long val;

    if (mach->bpval % 8 != 0) {
        return 0; // not supported with weird word sizes
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIGN, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NUMERIC, &lex, 1)) {
        /* XXX error condition */
    }
    val = lexeme_signedval(lex);
    lexeme_free(lex);
    if (val < 0 || val > machine_align_max(mach)) {
        /* XXX error condition */
        *valp = 0;
    } else {
        *valp = (int)val;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }
    return 1;
}

static int
attr_initial (expr_ctx_t ctx, int defau, initval_t **ivlistp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_INITIAL, 0, 1)) {
        return 0;
    }
    *ivlistp = plit_items(ctx, defau);
    return 1;
}

static int
attr_field (expr_ctx_t ctx, scopectx_t scope, lexseq_t *fldset)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexeme_t *lex;
    strdesc_t *text;
    name_t *np;
    lextype_t ftypes[2] = { LEXTYPE_NAME_FIELD, LEXTYPE_NAME_FIELDSET };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_FIELD, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    while (1) {
        if (!parser_expect_oneof(pctx, QL_NORMAL, ftypes, 2, &lex, 1)) {
            /* XXX error condition */
        } else {
            text = lexeme_text(lex);
            np = name_search(scope, text->ptr, text->len, 0);
            if (!np || (name_type(np) != LEXTYPE_NAME_FIELD &&
                        name_type(np) != LEXTYPE_NAME_FIELDSET)) {
                /* XXX error condition */
            } else if (name_type(np) == LEXTYPE_NAME_FIELD) {
                lexseq_instail(fldset, lexeme_copy(lex));
            } else {
                lexseq_copy(fldset, name_data_lexseq(np));
            }
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }

    return 1;

} /* attr_field */

static int
attr_preset (expr_ctx_t ctx, name_t *np,
             seg_t *seg, symbol_t *ni, strudef_t *stru,
             initval_t **ivlistp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    lexeme_t *lex;
    initval_t *ivlist = 0;
    expr_node_t *pexp, *exp;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_PRESET, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    if (np == 0) {
        /* previous error condition - just skip processing */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    while (1) {
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
        }
        // We're essentially building an assignment expression that
        // will get interpreted at a later stage.
        lex = parser_lexeme_create(pctx, LEXTYPE_NAME_DATA, name_string(np));
        lexeme_type_set(lex, LEXTYPE_NAME_DATA);
        lexeme_ctx_set(lex, np);
        if (structure_reference(ctx, stru, 1, ni, lex) != 0) {
            /* XXX error condition */
        }
        // Should now have a STRUREF expression
        pexp = lexeme_ctx_get(lex);
        lexeme_free(lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            /* XXX error condition */
        }
        exp = 0;
        if (!expr_expr_next(ctx, &exp)) {
            /* XXX error condition */
        }
        if (exp != 0 && expr_type(exp) == EXPTYPE_PRIM_LIT) {
            ivlist = preset_scalar_add(stg, ivlist, pexp, expr_litval(exp));
            expr_node_free(ctx, exp);
            exp = 0;
        }
        if (exp != 0) {
            if (seg_type(seg) == SEGTYPE_STATIC && !expr_is_ltce(exp)) {
                /* XXX error condition */
            }
            ivlist = preset_expr_add(stg, ivlist, pexp, 1, exp);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    } /* while */

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }

    *ivlistp = ivlist;
    return 1;
    
} /* attr_preset */

static int
handle_data_attrs (expr_ctx_t ctx, scopectx_t scope, decltype_t dt,
                   seg_t *seg, symbol_t *ni, name_t *np, int is_bind) {
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    int saw_au, saw_ext, saw_align, saw_init, saw_vol,
        saw_alias, saw_psect, saw_stru, saw_field, saw_preset;
    int did1;
    int align;
    unsigned int stru_units;
    scalar_attr_t scattr;
    initval_t *ivlist;
    strudef_t *stru;
    lexseq_t fldset;
    scopectx_t struscope;
    psect_t *psect;

    saw_au = saw_ext = saw_align = saw_init = saw_vol = saw_alias = saw_psect = 0;
    saw_stru = 0;
    saw_field = saw_preset = -1; // not allowed until saw_stru

    scattr_units_set(&scattr, machine_scalar_units(mach));
    scattr_signed_set(&scattr, 0);

    if (is_bind || dt == DCL_MAP) {
        saw_init = saw_psect = saw_align = -1;
        saw_preset = -2;
    } else {
        if (seg_type(seg) == SEGTYPE_REGISTER) {
            saw_align = saw_vol = saw_alias = -1;
        } else if (seg_type(seg) == SEGTYPE_STACK) {
            saw_psect = -1;
        } else {
            if (dt == DCL_FORWARD || dt == DCL_EXTERNAL) {
                saw_align = saw_init = -1;
            }
        }
    }


    ivlist = 0;
    lexseq_init(&fldset);
    parser_set_indecl(pctx, 1);

    do {
        did1 = 0;
        if (!saw_au) {
            saw_au = attr_allocunit(ctx, (int *)&scattr.units);
            if (saw_au) { did1 = 1; saw_stru = -1; }
        }
        if (!saw_ext) {
            saw_ext = attr_extension(ctx, &scattr.signext);
            if (saw_ext) { did1 = 1; saw_stru = -1; }
        }
        if (!saw_stru) {
            lexeme_t *lex;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REF, 0, 1)) {
                symbol_data_flags_set(ni, symbol_data_flags(ni) | SYM_M_REF);
                if (!parser_expect(pctx, QL_NORMAL,
                                   LEXTYPE_NAME_STRUCTURE, &lex, 1)) {
                    /* XXX error condition */
                }
                saw_stru = 1;
            } else {
                saw_stru = parser_expect(pctx, QL_NORMAL,
                                         LEXTYPE_NAME_STRUCTURE, &lex, 1);
            }
            if (saw_stru) {
                structure_allocate(ctx, lexeme_ctx_get(lex), &stru,
                                   &stru_units, &struscope);
                saw_au = saw_ext = -1;
                symbol_data_struc_set(ni, stru);
                symbol_data_struscope_set(ni, struscope);
                if (saw_preset == -1) saw_preset = 0;
                if (saw_field == -1) saw_field = 0;
                did1 = 1;
            }
        }
        if (!saw_field) {
            saw_field = attr_field(ctx, scope, &fldset);
            if (saw_field) {
                symbol_data_fields_set(ni, &fldset);
                did1 = 1;
            }
        }
        if (!saw_align) {
            saw_align = attr_align(ctx, &align);
            if (saw_align) did1 = saw_align;
        }
        if (!saw_psect) {
            saw_psect = attr_psect(ctx, &psect);
            if (saw_psect) did1 = saw_psect;
        }
        if (!saw_init) {
            if (attr_initial(ctx, scattr_units(&scattr), &ivlist)) {
                saw_init = 1;
                saw_preset = -1;
                did1 = 1;
                if (saw_au == 0) saw_au = -1; // INITIAL must come after allocation-unit
            }
        }
        if (!saw_preset) {
            if (attr_preset(ctx, np, seg, ni, stru, &ivlist)) {
                saw_preset = 1;
                saw_init = -1;
                did1 = 1;
            }
        }
        if (!saw_vol) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_VOLATILE, 0, 1)) {
                did1 = 1;
                saw_vol = 1;
                saw_alias = -1;
                seg_flags_set(stg, seg, SEG_M_VOLATILE);
                symbol_data_flags_set(ni, symbol_data_flags(ni) | SYM_M_VOLATILE);
            }
        }
        if (!saw_alias) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIAS, 0, 1)) {
                did1 = 1;
                saw_vol = -1;
                saw_alias = 1;
                seg_flags_set(stg, seg, SEG_M_ALIAS);
                symbol_data_flags_set(ni, symbol_data_flags(ni) | SYM_M_ALIAS);
            }
        }

    } while (did1);

    parser_set_indecl(pctx, 0);

    // XXX need to compare attributes for FORWARD/EXTERNAL against
    //     their actual OWN/GLOBALs
    
    if (saw_psect > 0) seg_static_psect_set(stg, seg, psect);
    if (saw_align > 0) seg_alignment_set(stg, seg, align);
    if (saw_init > 0) seg_initval_set(stg, seg, ivlist);
    if (saw_preset > 0) seg_preset_set(stg, seg, ivlist);


    if (saw_stru > 0) {
        if (seg != 0) {
            if (symbol_data_flags(ni) & SYM_M_REF) {
                seg_size_set(stg, seg, machine_scalar_units(mach));
            } else {
                seg_size_set(stg, seg, stru_units);
            }
        }
    } else {
        if (saw_field > 0) {
            /* XXX error condition */
            lexseq_free(&fldset);
        }
        if (saw_au > 0 && seg != 0) seg_size_set(stg, seg, scattr.units);
        symbol_data_scattr_set(ni, &scattr);
    }

    return 1;

} /* handle_data_attrs */

static int
declare_data (expr_ctx_t ctx, scopectx_t scope,
              symtype_t symtype, lextype_t lt, decltype_t dt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    declctx_t dctx = expr_decl_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    psect_t *psect = 0;
    seg_t *seg;
    segtype_t st;
    int status = 1;
    int stackonly = 0;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    if (symtype == SYMTYPE_DATASEG) {
        stackonly = (lt == LEXTYPE_DCL_STACKLOCAL);
        st = (lt == LEXTYPE_DCL_LOCAL || lt == LEXTYPE_DCL_STACKLOCAL) ?
                SEGTYPE_STACK : SEGTYPE_STATIC;
    } else if (symtype == SYMTYPE_REGISTER) {
        st = SEGTYPE_REGISTER;
    }
    if (st == SEGTYPE_STATIC) {
        if (dt == DCL_NORMAL) {
            np = scope_sclass_psectname(scope, SCLASS_OWN);
            psect = name_data_ptr(np);
        } else if (dt == DCL_GLOBAL) {
            np = scope_sclass_psectname(scope, SCLASS_GLOBAL);
            psect = name_data_ptr(np);
        }
    }
    while (status != 0) {
        symbol_t *ni;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        ni = symbol_alloc(ctx, symtype);
        np = name_declare(scope, namestr->ptr, namestr->len, LEXTYPE_NAME_DATA, pos);
        if (np == 0) {
            /* XXX error condition */
        } else {
            name_data_set_ptr(np, ni);
        }
        if (st == SEGTYPE_STACK) {
            seg = seg_alloc_stack(stg, pos, stackonly);
        } else if (st == SEGTYPE_REGISTER) {
            seg = seg_alloc_register(stg, pos);
        } else {
            seg = seg_alloc_static(stg, pos, psect);
        }
        symbol_data_seg_set(ni, seg);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, seg, ni, np, 0);
            if (!status) {
                /* XXX error condition */
            }
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status >= 0 && (dt == DCL_EXTERNAL || dt == DCL_GLOBAL)) {
            name_t *extnp = name_search(dctx->globals, namestr->ptr,
                                        namestr->len, 1);
            if (extnp == 0) {
                /* XXX error condition */
                status = -1;
            } else {
                symbol_t *extsym;
                if (name_type(extnp) == LEXTYPE_NAME) {
                    extsym = symbol_alloc(ctx, SYMTYPE_DATASEG);
                    if (dt == DCL_GLOBAL) {
                        symbol_data_seg_set(extsym, seg);
                        symbol_data_struc_set(extsym, symbol_data_struc(ni));
                        symbol_data_fields_set(extsym, symbol_data_fields(ni));
                        symbol_data_scattr_set(extsym, symbol_data_scattr(ni));
                        symbol_data_flags_set(extsym, symbol_data_flags(ni));
                    }
                    extnp = name_declare(dctx->globals, namestr->ptr,
                                         namestr->len, LEXTYPE_NAME_DATA, pos);
                    name_data_set_ptr(extnp, extsym);
                } else {
                    if (dt == DCL_GLOBAL) {
                        /* XXX error - redeclaration */
                    }
                    extsym = name_data_ptr(extnp);
                    if (symbol_type(extsym) != SYMTYPE_DATASEG) {
                        /* XXX error condition */
                        return 0;
                    }
                    if (dt == DCL_EXTERNAL) {
                        // XXX - need to compare attributes
                    }
                }
            }
        }
        if (status < 0) {
            symbol_free(ctx, ni);
            seg_free(stg, seg);
            string_free(namestr);
            if (np != 0) name_undeclare(scope, np);
            /* XXX error condition */
            break;
        }
        seg_commit(stg, seg);
        string_free(namestr);
    }

    return 1;
}

/*
 * declare_bind
 */
static int
declare_bind (expr_ctx_t ctx, scopectx_t scope, decltype_t dt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    seg_t *seg;
    initval_t *ivlist;
    int status = 1;
    expr_node_t *exp;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    while (status != 0) {
        symbol_t *ni;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            /* XXX error condition */
            return 0;
        }
        if (!expr_expr_next(ctx, &exp)) {
            /* XXX error condition */
            return 0;
        }
        // XXX manual says allowed LTCEs are more restrictive
        //     than in other contexts?
        if (dt == DCL_GLOBAL && !expr_is_ltce(exp)) {
            /* XXX error condition */
        }
        ni = symbol_alloc(ctx, SYMTYPE_DATASEG);
        // Allocate storage for the BIND pointer.  If we're bound
        // to an LTCE, make it static storage (with a global symbol
        // if it's a GLOBAL BIND).  Otherwise, it's local storage.
        if (expr_is_ltce(exp)) {
            psect_t *psect = 0;
            if (dt != DCL_GLOBAL) {
                np = scope_sclass_psectname(scope, SCLASS_PLIT);
                psect = name_data_ptr(np);
            }
            seg = seg_alloc_static(stg, pos, psect);
        } else {
            seg = seg_alloc_stack(stg, pos, 0);
        }
        ivlist = expr_initval_add(ctx, 0, exp, machine_scalar_units(mach));
        seg_initval_set(stg, seg, ivlist);

        np = name_declare(scope, namestr->ptr, namestr->len, LEXTYPE_NAME_DATA, pos);
        if (np == 0) {
            /* XXX error condition */
        } else {
            name_data_set_ptr(np, ni);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, seg, ni, np, 1);
            if (!status) {
                /* XXX error condition */
            }
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status < 0) {
            symbol_free(ctx, ni);
            string_free(namestr);
            seg_free(stg, seg);
            if (np != 0) name_undeclare(scope, np);
            /* XXX error condition */
            break;
        }
        seg_commit(stg, seg);
        symbol_data_seg_set(ni, seg);
        string_free(namestr);
    }

    return 1;
    
} /* declare_bind */

/*
 * declare_map
 */
static int
declare_map (expr_ctx_t ctx, scopectx_t scope)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    int status = 1;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    while (status != 0) {
        symbol_t *ni;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        np = name_search(scope, namestr->ptr, namestr->len, 0);
        if (name_type(np) != LEXTYPE_NAME_DATA) {
            /* XXX error condition */
            return 0;
        }
        ni = name_data_ptr(np);
        if (symbol_type(ni) != SYMTYPE_DATASEG) {
            /* XXX error condition */
            return 0;
        }
        if (name_scope(np) != scope) {
            np = name_declare(scope, namestr->ptr, namestr->len, LEXTYPE_NAME_DATA, pos);
            ni = symbol_copy(ctx, ni);
            name_data_set_ptr(np, ni);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, DCL_MAP, 0, ni, np, 0);
            if (!status) {
                /* XXX error condition */
            }
        } else {
            /* XXX error condition */
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status < 0) {
            string_free(namestr);
            /* XXX error condition */
            break;
        }
        string_free(namestr);
    }

    return 1;
    
} /* declare_map */

/*
 * parse_formals
 *
 * Parse a routine's formal parameter list.
 *
 * Returns: 1 on success
 *          0 on failure
 */
static int
parse_formals (expr_ctx_t ctx, scopectx_t curscope,
               scopectx_t *argtable, arglist_t *inargs, arglist_t *outargs)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    arglist_t *curargs;
    symbol_t *ni;
    int which;
    static lextype_t delims[3] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_SEMI,
                                   LEXTYPE_DELIM_COMMA };


    inargs->count = outargs->count = 0;
    curargs = inargs;

    curscope = parser_scope_begin(pctx);

    while (1) {
        if (parse_decl_name(pctx, curscope, &namestr, &pos)) {
            if (*argtable == 0) {
                *argtable = scope_begin(0);
            }
            curargs->arg[curargs->count] = name_declare(*argtable,
                                                        namestr->ptr,
                                                        namestr->len,
                                                        LEXTYPE_NAME_DATA,
                                                        pos);
            ni = symbol_alloc(ctx, SYMTYPE_DATASEG);
            if (ni == 0) {
                /* XXX error condition */
                break;
            }
            name_data_set_ptr(curargs->arg[curargs->count], ni);
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                if (!handle_data_attrs(ctx, curscope, DCL_MAP, 0, ni, 0, 0)) {
                    /* XXX error condition */
                }
            }
        } else {
            curargs->arg[curargs->count] = 0;
        }
        curargs->count += 1;
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 3, 0, 1);
        if (which == 1) { // semicolon
            curargs = outargs;
            continue;
        }
        if (which != 2) {
            break;
        }
    }
    curscope = parser_scope_end(pctx);

    return (which == 0);
    
} /* parse_formals */

static int
handle_routine_attrs (expr_ctx_t ctx, scopectx_t scope,
                      decltype_t dt, seg_t *seg, symbol_t *ni,
                      int is_bind)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    int saw_psect, saw_novalue; // XXX need to handle linkage here
    int did1;
    int which;
    psect_t *psect;
    static lextype_t delims[3] = { LEXTYPE_DELIM_SEMI,
                                   LEXTYPE_DELIM_COMMA,
                                   LEXTYPE_OP_ASSIGN};

    saw_psect = saw_novalue = 0;

    if (is_bind) {
        saw_psect = -1;
    }

    symbol_data_seg_set(ni, seg);

    do {
        did1 = 0;
        if (!saw_psect) {
            saw_psect = attr_psect(ctx, &psect);
            if (saw_psect) did1 = saw_psect;
        }
        if (!saw_novalue) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_NOVALUE, 0, 1)) {
                did1 = 1;
                saw_novalue = 1;
                symbol_data_flags_set(ni, symbol_data_flags(ni) | SYM_M_NOVALUE);
            }
        }
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 3, 0, 1);

    } while (which < 0 && did1);

    if (which < 0) {
        /* XXX error condition - unknown attribute */
        return -1;
    }

    // XXX need to compare attributes for FORWARD/EXTERNAL against
    //     their actual OWN/GLOBALs

    if (saw_psect > 0) seg_static_psect_set(expr_stg_ctx(ctx), seg, psect);

    return which;
    
} /* handle_routine_attrs */

/*
 * declare_routine
 */
static int
declare_routine (expr_ctx_t ctx, scopectx_t scope, decltype_t dt, int is_bind)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    declctx_t dctx = expr_decl_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    scopectx_t argscope;
    symbol_t *ni;
    seg_t *seg;
    psect_t *psect;
    name_t *np;
    int which;

    if (!is_bind) {
        np = scope_sclass_psectname(scope, SCLASS_CODE);
        psect = name_data_ptr(np);
    } else if (dt == DCL_GLOBAL) {
        np = scope_sclass_psectname(scope, SCLASS_GLOBAL);
        psect = name_data_ptr(np);
    }

    while (1) {
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        ni = symbol_alloc(ctx, SYMTYPE_ROUTINE);
        if (is_bind) {
            if (dt == DCL_GLOBAL) {
                seg = seg_alloc_static(stg, pos, psect);
            } else {
                seg = seg_alloc_stack(stg, pos, 0);
            }
        } else {
            seg = seg_alloc_static(stg, pos, psect);
        }
        argscope = 0;
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
            if (!parse_formals(ctx, scope, &argscope,
                               symbol_routine_inpargs(ni),
                               symbol_routine_outargs(ni))) {
                /* XXX error condition */
                if (ni != 0) symbol_free(ctx, ni);
                if (seg != 0) seg_free(stg, seg);
                return 0;
            }
        }
        symbol_routine_argscope_set(ni, argscope);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            which = handle_routine_attrs(ctx, scope, dt, seg, ni, is_bind);
            if (which < 0) {
                /* XXX error condition */
                if (ni != 0) symbol_free(ctx, ni);
                if (seg != 0) seg_free(stg, seg);
                return 0;
            }
        }
        if (is_bind || dt == DCL_NORMAL || dt == DCL_GLOBAL) {
            expr_node_t *exp;
            scopectx_t pscope;

            if (which != 2) { // i.e., the '='
                /* XXX error condition - need expression */
                if (ni != 0) symbol_free(ctx, ni);
                if (seg != 0) seg_free(stg, seg);
                return 0;
            }
            if (!is_bind) {
                int i;
                frame_t *frame = frame_begin(stg, pos, ni);
                arglist_t *inargs = symbol_routine_inpargs(ni);
                symbol_routine_stack_set(ni, frame);
                for (i = 0; i < inargs->count; i++) {
                    seg_t *seg;
                    if (inargs->arg[i] == 0) continue;
                    seg = seg_alloc_stack(stg, pos, 0);
                    seg_size_set(stg, seg, machine_scalar_units(mach));
                    seg_commit(stg, seg);
                    symbol_data_seg_set(name_data_ptr(inargs->arg[i]), seg);
                }
                // XXX insert linkage handling here
                pscope = scope_copy(argscope, 0);
                parser_scope_set(pctx, pscope);
            }
            if (!expr_expr_next(ctx, &exp)) {
                /* XXX error condition */
            }
            if (is_bind) {
                initval_t *ivlist;
                ivlist = initval_expr_add(stg, 0, 1, 1, exp, 32, 0);
                seg_initval_set(stg, seg, ivlist);
                symbol_routine_seg_set(ni, seg);
            } else {
                parser_scope_end(pctx);
                frame_end(stg);
                symbol_routine_expr_set(ni, exp);
                symbol_routine_seg_set(ni, seg);
            }
        }
        if (seg != 0) seg_commit(stg, seg);
        if (dt == DCL_EXTERNAL || dt == DCL_GLOBAL) {
            name_t *extnp = name_search(dctx->globals, namestr->ptr,
                                        namestr->len, 1);
            if (extnp == 0) {
                /* XXX error condition */
                break;
            } else {
                symbol_t *extsym;
                if (name_type(extnp) == LEXTYPE_NAME) {
                    extsym = symbol_alloc(ctx, SYMTYPE_ROUTINE);
                    if (dt == DCL_GLOBAL) {
                        symbol_routine_seg_set(extsym, symbol_routine_seg(ni));
                        symbol_routine_expr_set(extsym, symbol_routine_expr(ni));
                        symbol_routine_argscope_set(extsym, symbol_routine_argscope(ni));
                        // XXX and the argument lists
                    }
                    extnp = name_declare(dctx->globals, namestr->ptr,
                                         namestr->len, LEXTYPE_NAME_DATA, pos);
                    name_data_set_ptr(extnp, extsym);
                } else {
                    if (dt == DCL_GLOBAL) {
                        /* XXX error - redeclaration */
                    }
                    extsym = name_data_ptr(extnp);
                    if (symbol_type(extsym) != SYMTYPE_ROUTINE) {
                        /* XXX error condition */
                        return 0;
                    }
                    if (dt == DCL_EXTERNAL) {
                        // XXX - need to compare attributes
                    }
                }
            }
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
            break;
        }
    }
    return 1;
}

/*
 * declare_require
 */
static int
declare_require (parse_ctx_t pctx)
{
    lexeme_t *lex;
    strdesc_t *str;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        /* XXX errror condition */
    }
    str = lexeme_text(lex);
    if (!parser_fopen(pctx, str->ptr, str->len)) {
        /* XXX error condition */
    }
    lexeme_free(lex);
    return 1;

} /* declare_require */

/*
 * undeclare
 */
static int
undeclare (parse_ctx_t pctx, scopectx_t scope)
{
    lexeme_t *lex;
    strdesc_t *str;
    name_t *np;

    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &lex, 1)) {
            /* XXX error condition */
        } else {
            str = lexeme_text(lex);
            np = name_search(scope, str->ptr, str->len, 0);
            // nonexistent, or reserved, or already undeclared,
            // or not an actual name, or not DECLAREd
            if (np == 0 || name_type(np) < LEXTYPE_NAME_MIN+1
                || name_type(np) > LEXTYPE_NAME_MAX
                || (name_flags(np) & NAME_M_RESERVED) != 0
                || (name_flags(np) & NAME_M_DECLARED) == 0 ||
                !name_undeclare(scope, np)) {
                /* XXX error condition */
            }
        }
        lexeme_free(lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        /* XXX error condition */
    }
    return 1;
    
} /* undeclare */
/*
 * bind_data
 */
static expr_node_t *
bind_data (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    name_t *np = lexeme_ctx_get(lex);
    expr_node_t *exp;
    symbol_t *ni;

    if (np == 0) {
        return 0;
    }
    ni = name_data_ptr(np);
    if (ni == 0) {
        return 0;
    }

    exp = 0;
    switch (symbol_type(ni)) {
        case SYMTYPE_DATASEG: {
            strudef_t *stru = symbol_data_struc(ni);
            if (stru != 0 &&
                parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
                exp = structure_reference(ctx, stru, 0, ni, lex);
            } else {
                scalar_attr_t  *sa;
                exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG,
                                      lexeme_textpos_get(lex));
                expr_seg_name_set(exp, lexeme_ctx_get(lex));
                expr_seg_base_set(exp, symbol_data_seg(ni));
                sa = symbol_data_scattr(ni);
                expr_seg_units_set(exp, sa->units);
                expr_seg_signext_set(exp, sa->signext);
                expr_is_ltce_set(exp, seg_addr_is_ltce(symbol_data_seg(ni)));
            }
            break;
        }

        case SYMTYPE_ROUTINE:
            exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG,
                                  lexeme_textpos_get(lex));
            expr_seg_name_set(exp, lexeme_ctx_get(lex));
            expr_seg_base_set(exp, symbol_routine_seg(ni));
            expr_seg_units_set(exp, machine_scalar_units(mach));
            expr_seg_signext_set(exp, machine_addr_signed(mach));
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                exp = expr_parse_arglist(ctx, exp);
            }
            break;

        default:
            break;
    }

    return exp;

} /* bind_data */



/*
 * psects_init
 *
 * Set up the default PSECTs and their predeclared names.
 * XXX this should probably be machine-specific.
 */
static void
psects_init (scopectx_t kwdscope, stgctx_t stg, machinedef_t *mach) {

    psect_t *ps;
    name_t *np;
    strdesc_t own = STRDEF("$OWN$");
    strdesc_t global = STRDEF("$GLOBAL$");
    strdesc_t plit = STRDEF("$PLIT$");
    strdesc_t code = STRDEF("$CODE$");

    ps = psect_create(stg, &own, 0, PSECT_M_ATTR_WRITE);
    np = name_declare(kwdscope, own.ptr, own.len, LEXTYPE_NAME_PSECT, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_OWN, np);
    name_data_set_ptr(np, ps);

    ps = psect_create(stg, &global, 0, PSECT_M_ATTR_WRITE);
    np = name_declare(kwdscope, global.ptr, global.len, LEXTYPE_NAME_PSECT, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_GLOBAL, np);
    name_data_set_ptr(np, ps);

    ps = psect_create(stg, &plit, 0, 0);
    np = name_declare(kwdscope, plit.ptr, plit.len, LEXTYPE_NAME_PSECT, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_PLIT, np);
    name_data_set_ptr(np, ps);

    ps = psect_create(stg, &code, 0, PSECT_M_ATTR_EXEC);
    np = name_declare(kwdscope, code.ptr, code.len, LEXTYPE_NAME_PSECT, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_CODE, np);
    name_data_set_ptr(np, ps);

} /* psects_init */

/*
 * declarations_init
 *
 * Initialization routine.
 */
void *
declarations_init (expr_ctx_t ctx, parse_ctx_t pctx,
                   scopectx_t kwdscope,
                   stgctx_t stg, machinedef_t *mach)
{
    int i;
    name_t *np;
    symbol_t *ni;
    declctx_t dctx;

    dctx = malloc(sizeof(struct declctx_s));
    if (dctx == 0) {
        return 0;
    }
    memset(dctx, 0, sizeof(struct declctx_s));
    dctx->globals = scope_begin(0);
    expr_decl_ctx_set(ctx, dctx);

    for (i = 0; i < sizeof(decl_names)/sizeof(decl_names[0]); i++) {
        name_insert(kwdscope, &decl_names[i]);
    }

    lextype_register(LEXTYPE_NAME_COMPILETIME, bind_compiletime);
    lextype_register(LEXTYPE_NAME_LITERAL, bind_literal);
    expr_dispatch_register(ctx, LEXTYPE_NAME_DATA, bind_data);
    macros_init(kwdscope);
    psects_init(kwdscope, stg, mach);
    np = name_declare(kwdscope, "%BPUNIT", 7, LEXTYPE_NAME_LITERAL, 0);
    if (np != 0) {
        name_flags_set(np, NAME_M_RESERVED);
        ni = symbol_alloc(ctx, SYMTYPE_LITERAL);
        if (ni != 0) {
            symbol_lit_signextend_set(ni, 0);
            symbol_lit_valwidth_set(ni, machine_scalar_bits(mach));
            symbol_lit_val_set(ni, machine_unit_bits(mach));
            name_data_set_ptr(np, ni);
        }
    }
    np = name_declare(kwdscope, "%BPADDR", 7, LEXTYPE_NAME_LITERAL, 0);
    if (np != 0) {
        name_flags_set(np, NAME_M_RESERVED);
        ni = symbol_alloc(ctx, SYMTYPE_LITERAL);
        if (ni != 0) {
            symbol_lit_signextend_set(ni, 0);
            symbol_lit_valwidth_set(ni, machine_scalar_bits(mach));
            symbol_lit_val_set(ni, machine_addr_bits(mach));
            name_data_set_ptr(np, ni);
        }
    }
    np = name_declare(kwdscope, "%BPVAL", 6, LEXTYPE_NAME_LITERAL, 0);
    if (np != 0) {
        name_flags_set(np, NAME_M_RESERVED);
        ni = symbol_alloc(ctx, SYMTYPE_LITERAL);
        if (ni != 0) {
            symbol_lit_signextend_set(ni, 0);
            symbol_lit_valwidth_set(ni, machine_scalar_bits(mach));
            symbol_lit_val_set(ni, machine_scalar_bits(mach));
            name_data_set_ptr(np, ni);
        }
    }
    np = name_declare(kwdscope, "%UPVAL", 6, LEXTYPE_NAME_LITERAL, 0);
    if (np != 0) {
        name_flags_set(np, NAME_M_RESERVED);
        ni = symbol_alloc(ctx, SYMTYPE_LITERAL);
        if (ni != 0) {
            symbol_lit_signextend_set(ni, 0);
            symbol_lit_valwidth_set(ni, machine_scalar_bits(mach));
            symbol_lit_val_set(ni, machine_scalar_units(mach));
            name_data_set_ptr(np, ni);
        }
    }
    structures_init(ctx);

    return dctx;

} /* declarations_init */

/*
 * parse_declaration
 */
int
parse_declaration (expr_ctx_t ctx)
{
    lextype_t lt;
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    scopectx_t scope = parser_scope_get(pctx);
    int status;
    int which;
    static lextype_t pfx[] = { LEXTYPE_DCL_GLOBAL, LEXTYPE_DCL_EXTERNAL,
                               LEXTYPE_DCL_FORWARD };
    static decltype_t  dtypes[] = { DCL_NORMAL, DCL_GLOBAL, DCL_EXTERNAL, DCL_FORWARD };
    static lextype_t allowed[3][4] = {
        { LEXTYPE_DCL_LITERAL, LEXTYPE_DCL_ROUTINE, LEXTYPE_DCL_REGISTER, LEXTYPE_DCL_BIND },
        { LEXTYPE_DCL_LITERAL, LEXTYPE_DCL_ROUTINE, LEXTYPE_DCL_REGISTER },
        { LEXTYPE_DCL_ROUTINE }
    };
    static int count[3] = { 4, 3, 1 };

    which = parser_expect_oneof(pctx, QL_NORMAL, pfx, 3, 0, 1);
    if (which >= 0) {
        int i;
        i = parser_expect_oneof(pctx, QL_NORMAL, allowed[which], count[which], 0, 1);
        if (i < 0) {
            lt = pfx[which];
        } else {
            lt = allowed[which][i];
        }
        which += 1;
    } else {
        which = 0;
        lt = parser_next(pctx, QL_NORMAL, 0);
    }

    switch (lt) {
        case LEXTYPE_DCL_BIND:
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_ROUTINE, 0, 1)) {
                status = declare_routine(ctx, scope, dtypes[which], 1);
            } else {
                status = declare_bind(ctx, scope, dtypes[which]);
            }
            break;
        case LEXTYPE_DCL_GLOBAL:
        case LEXTYPE_DCL_EXTERNAL:
        case LEXTYPE_DCL_OWN:
        case LEXTYPE_DCL_FORWARD:
            status = declare_data(ctx, scope, SYMTYPE_DATASEG, lt, dtypes[which]);
            break;
        case LEXTYPE_DCL_STACKLOCAL:
        case LEXTYPE_DCL_LOCAL:
            status = declare_data(ctx, scope, SYMTYPE_DATASEG, lt, DCL_NORMAL);
            break;
        case LEXTYPE_DCL_REGISTER:
            status = declare_data(ctx, scope, SYMTYPE_REGISTER, lt, dtypes[which]);
            break;
        case LEXTYPE_DCL_MACRO:
        case LEXTYPE_DCL_KEYWORDMACRO:
            status = declare_macro(pctx, scope, lt);
            break;
        case LEXTYPE_DCL_COMPILETIME:
            status = declare_compiletime(ctx, scope, lt);
            break;
        case LEXTYPE_DCL_LITERAL:
            status = declare_literal(ctx, scope, dtypes[which]);
            break;
        case LEXTYPE_DCL_LABEL:
            status = declare_label(pctx, scope);
            break;
        case LEXTYPE_DCL_PSECT:
            status = declare_psect(ctx, scope);
            break;
        case LEXTYPE_DCL_STRUCTURE:
            status = declare_structure(ctx, scope);
            break;
        case LEXTYPE_DCL_MAP:
            status = declare_map(ctx, scope);
            break;
        case LEXTYPE_DCL_FIELD:
            status = declare_field(ctx, scope);
            break;
        case LEXTYPE_DCL_REQUIRE:
            status = declare_require(pctx);
            break;
        case LEXTYPE_DCL_UNDECLARE:
            status = undeclare(pctx, scope);
            break;
        case LEXTYPE_DCL_ROUTINE:
            status = declare_routine(ctx, scope, dtypes[which], 0);
            break;
        default:
            /* XXX error condition */
            status = 0;
            break;
    }

    return status;

} /* parse_declaration */

/*
 * declare_module
 */
int
declare_module (expr_ctx_t ctx)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
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
                      lexeme_textpos_get(lex));
    lexeme_free(lex);
    if (np == 0) {
        /* XXX error condition */
        return 0;
    }

    // XXX need to handle module-switches here
    
    if (!expr_parse_block(ctx, &blkexp)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_ELUDOM, 0, 0)) {
        /* XXX error condition */
        // but ignore the error
    }

    name_data_set_ptr(np, blkexp);

    return 1;

} /* declare_module */