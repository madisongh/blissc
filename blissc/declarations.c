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
#include "macros.h"
#include "storage.h"
#include "strings.h"
#include "utils.h"

typedef enum {
    DCL_NORMAL = 0,
    DCL_GLOBAL,
    DCL_EXTERNAL,
    DCL_FORWARD
} decltype_t;


#define ALLOC_QTY 128
static nameinfo_t *freenis = 0;

static name_t decl_names[] = {
    NAMEDEF("LITERAL", LEXTYPE_DCL_LITERAL, NAME_M_RESERVED),
    NAMEDEF("GLOBAL", LEXTYPE_DCL_GLOBAL, NAME_M_RESERVED),
    NAMEDEF("EXTERNAL", LEXTYPE_DCL_EXTERNAL, NAME_M_RESERVED),
    NAMEDEF("FORWARD", LEXTYPE_DCL_FORWARD, NAME_M_RESERVED),
    NAMEDEF("COMPILETIME", LEXTYPE_DCL_COMPILETIME, NAME_M_RESERVED),
    NAMEDEF("MODULE", LEXTYPE_DCL_MODULE, NAME_M_RESERVED),
    NAMEDEF("ELUDOM", LEXTYPE_DCL_ELUDOM, NAME_M_RESERVED),
    NAMEDEF("ROUTINE", LEXTYPE_DCL_ROUTINE, NAME_M_RESERVED),
    NAMEDEF("LABEL", LEXTYPE_DCL_LABEL, NAME_M_RESERVED),
    NAMEDEF("LOCAL", LEXTYPE_DCL_LOCAL, NAME_M_RESERVED),
    NAMEDEF("OWN", LEXTYPE_DCL_OWN, NAME_M_RESERVED),
    NAMEDEF("STACKLOCAL", LEXTYPE_DCL_STACKLOCAL, NAME_M_RESERVED),
    NAMEDEF("STRUCTURE", LEXTYPE_DCL_STRUCTURE, NAME_M_RESERVED),
    NAMEDEF("LINKAGE", LEXTYPE_DCL_LINKAGE, NAME_M_RESERVED),
    NAMEDEF("BIND", LEXTYPE_DCL_BIND, NAME_M_RESERVED),
    NAMEDEF("PSECT", LEXTYPE_DCL_PSECT, NAME_M_RESERVED),
    NAMEDEF("BUILTIN", LEXTYPE_DCL_BUILTIN, NAME_M_RESERVED),
    NAMEDEF("UNDECLARE", LEXTYPE_DCL_UNDECLARE, NAME_M_RESERVED),
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
    NAMEDEF("PSECT", LEXTYPE_DCL_PSECT, NAME_M_RESERVED),
    NAMEDEF("CODE", LEXTYPE_KWD_CODE, NAME_M_RESERVED),
    NAMEDEF("NODEFAULT", LEXTYPE_KWD_NODEFAULT, NAME_M_RESERVED),
    NAMEDEF("WRITE", LEXTYPE_KWD_WRITE, 0),
    NAMEDEF("NOWRITE", LEXTYPE_KWD_NOWRITE, 0),
    NAMEDEF("EXECUTE", LEXTYPE_KWD_EXECUTE, 0),
    NAMEDEF("NOEXECUTE", LEXTYPE_KWD_NOEXECUTE, 0),
    NAMEDEF("OVERLAY", LEXTYPE_KWD_OVERLAY, 0),
    NAMEDEF("CONCATENATE", LEXTYPE_KWD_CONCATENATE, 0)
};

static int bind_compiletime(void *ctx, quotelevel_t ql, quotemodifier_t qm,
                            lextype_t lt, condstate_t cs, lexeme_t *lex,
                            lexseq_t *result);
static int declare_compiletime(parse_ctx_t pctx, scopectx_t scope,
                               lextype_t lt);

nameinfo_t *
nameinfo_alloc (nametype_t type)
{
    nameinfo_t *ni;
    int i;
    if (freenis == 0) {
        freenis = malloc(ALLOC_QTY*sizeof(nameinfo_t));
        if (freenis == 0) {
            return 0;
        }
        for (i = 0, ni = freenis; i < ALLOC_QTY-1; i++, ni++) {
            nameinfo_next_set(ni, ni+1);
        }
        nameinfo_next_set(ni, 0);
    }
    ni = freenis;
    freenis = nameinfo_next(ni);
    memset(ni, 0, sizeof(nameinfo_t));
    nameinfo_type_set(ni, type);
    return ni;

} /* nameinfo_alloc */

void
nameinfo_free (nameinfo_t *ni, stgctx_t stg)
{
    if (ni == 0) {
        return;
    }
    switch (nameinfo_type(ni)) {
        case NAMETYPE_LITERAL:
            break;
        case NAMETYPE_GLOBLIT:
        case NAMETYPE_EXTLIT:
            // hope we don't need this
            break;
        case NAMETYPE_OWN:
        case NAMETYPE_GLOBAL:
        case NAMETYPE_LOCAL:
        case NAMETYPE_STACKLOCAL:
        case NAMETYPE_FORWARD:
        case NAMETYPE_EXTERNAL:
            // hope we don't need to free the seg here
            // free struc, fields
            break;
        case NAMETYPE_REGISTER:
        case NAMETYPE_GLOBREG:
        case NAMETYPE_EXTREG:
            // free struc, fields
            break;
        case NAMETYPE_BIND:
        case NAMETYPE_GLOBBIND:
            expr_node_free(nameinfo_bind_expr(ni), stg);
            // free struc, fields
            break;
    }
    memset(ni, 0x42, sizeof(nameinfo_t));
    nameinfo_next_set(ni, freenis);
    freenis = ni;

} /* nameinfo_free */

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
    nameinfo_t *ni = name_data_ptr(np);
    nametype_t nt = nameinfo_type(ni);

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    if (nt == NAMETYPE_GLOBLIT || nt == NAMETYPE_EXTLIT) {
        lexeme_type_set(lex, LEXTYPE_SEGMENT);
        lexeme_ctx_set(lex, nameinfo_gxlit_seg(ni));
        return 0;
    }

    if (nt == NAMETYPE_LITERAL) {
        long val = getvalue(nameinfo_lit_val(ni),
                            nameinfo_lit_valwidth(ni),
                            nameinfo_lit_signextend(ni));
        lexeme_type_set(lex, LEXTYPE_NUMERIC);
        string_free(lexeme_text(lex));
        string_printf(lexeme_text(lex), "%ld", val);
        lexeme_val_setsigned(lex, val);
        return 0;
    }

    return -1; // should never happen

} /* bind_literal */

/*
 * parse_decl_name
 *
 * Common parsing logic for names to be declared.
 */
static int
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
declare_literal (parse_ctx_t pctx, scopectx_t scope, stgctx_t stg,
                 machinedef_t *mach, decltype_t decltype)
{
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    textpos_t pos;
    strdesc_t *namestr;
    long val;
    unsigned int range = machine_scalar_bits(mach);
    int is_signed = 1;
    nameinfo_t *ni;


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
                if (!parse_ctce(pctx, &lex)) {
                    /* XXX error condition */
                    string_free(namestr);
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
                long rval;
                is_signed = lt == LEXTYPE_ATTR_SIGNED;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    /* XXX error condition */
                }
                if (!parse_ctce(pctx, &lex)) {
                    /* XXX error condition */
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
        if (bits_needed(labs(val)) > machine_scalar_bits(mach) - is_signed) {
            /* XXX error condition */
        }
        ni = nameinfo_alloc((decltype == DCL_EXTERNAL ? NAMETYPE_EXTLIT
                             : (decltype == DCL_GLOBAL ? NAMETYPE_GLOBLIT
                                : NAMETYPE_LITERAL)));
        if (decltype == DCL_NORMAL) {
            nameinfo_lit_val_set(ni, val);
            nameinfo_lit_valwidth_set(ni, range);
            nameinfo_lit_signextend_set(ni, is_signed);
        } else {
            seg_t *seg;
            if (decltype == DCL_GLOBAL) {
                seg = seg_alloc_literal(stg, pos,
                                        getvalue(val, range, is_signed));
            } else {
                seg = 0;
            }
            nameinfo_gxlit_seg_set(ni, seg);
            nameinfo_gxlit_valwidth_set(ni, range);
            nameinfo_gxlit_signextend_set(ni, is_signed);
        }

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
declare_psect (parse_ctx_t pctx, scopectx_t scope)
{
    stgctx_t stg = parser_get_cctx(pctx);
    psect_t *ps;
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
            ps = name_data_ptr(np);
            if (has_attr) {
                // check for attribute compatibility XXX
            }
        } else {
            strdesc_t *text = lexeme_text(lex);
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
attr_psect (parse_ctx_t pctx, stgctx_t stg, psect_t **psp)
{
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

    return 1;

} /* attr_psect */

static int
attr_allocunit (parse_ctx_t pctx, machinedef_t *mach, int *valp)
{
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
plit_items (parse_ctx_t pctx, stgctx_t stg, int defau, machinedef_t *mach) {

    initval_t *ivlist, *iv;
    int itemau;
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    ivlist = 0;
    while (1) {
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REP, 0, 1)) {
            unsigned int repcount;
            if (!parse_ctce(pctx, &lex)) {
                /* XXX error condition */
                repcount = 1;
            } else {
                repcount = (unsigned int)lexeme_unsignedval(lex);
                lexeme_free(lex);
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
                /* XXX error condition */
            }
            if (attr_allocunit(pctx, mach, &itemau) == 0) {
                itemau = defau;
            }
            iv = plit_items(pctx, stg, itemau, mach);
            if (iv == 0) {
                /* XXX error condition */
            } else {
                ivlist = initval_ivlist_add(stg, ivlist, repcount, iv);
            }
        } else if (attr_allocunit(pctx, mach, &itemau) != 0) {
            iv = plit_items(pctx, stg, itemau, mach);
            if (iv == 0) {
                /* XXX error condition */
            } else {
                ivlist = initval_ivlist_add(stg, ivlist, 1, iv);
            }
        } else {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
                ivlist = initval_string_add(stg, ivlist, 1, lexeme_text(lex));
                lexeme_free(lex);
            } else if (parser_expect(pctx, QL_NORMAL, LEXTYPE_NUMERIC, &lex, 1)) {
                long val = lexeme_signedval(lex);
                if (bits_needed(val) > itemau<<3) {
                    /* XXX error condition */
                }
                ivlist = initval_scalar_add(stg, ivlist, 1, val, itemau<<3, 0);
                lexeme_free(lex);
                /* XXX LTCE also allowed here */
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
define_plit (parse_ctx_t pctx, stgctx_t stg, lextype_t curlt)
{
    machinedef_t *mach = parser_get_cctx(pctx);
    int plitau;
    seg_t *seg;
    psect_t *psect;
    initval_t *ivlist;

    if (!attr_psect(pctx, stg, &psect)) {
        name_t *np = scope_sclass_psectname(parser_scope_get(pctx), SCLASS_PLIT);
        psect = name_data_ptr(np);
    }
    if (!attr_allocunit(pctx, mach, &plitau)) {
        plitau = machine_scalar_units(mach);
    }
    ivlist = plit_items(pctx, stg, plitau, mach);
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
}

static int
attr_extension (parse_ctx_t pctx, machinedef_t *mach, int *signext)
{
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
attr_align (parse_ctx_t pctx, machinedef_t *mach, int *valp)
{
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
    if (val < 0 || val > mach->max_align) {
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
attr_initial (parse_ctx_t pctx, stgctx_t stg, int defau, machinedef_t *mach,
              initval_t **ivlistp)
{
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_INITIAL, 0, 1)) {
        return 0;
    }
    *ivlistp = plit_items(pctx, stg, defau, mach);
    return 1;
}

static int
handle_static_attrs (parse_ctx_t pctx, scopectx_t scope, stgctx_t stg,
                     machinedef_t *mach, decltype_t dt, seg_t *seg,
                     nameinfo_t *ni) {
    int saw_au, saw_ext, saw_align, saw_init, saw_vol,
        saw_alias, saw_psect;
    int which;
    int align;
    scalar_attr_t scattr;
    initval_t *ivlist;
    psect_t *psect;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    saw_au = saw_ext = saw_align = saw_init = saw_vol = saw_alias = saw_psect = 0;

    scattr_units_set(&scattr, machine_scalar_units(mach));
    scattr_signed_set(&scattr, 0);

    if (dt == DCL_FORWARD || dt == DCL_EXTERNAL) {
        saw_align = saw_init = -1;
    }

    ivlist = 0;

    do {
        if (!saw_au) saw_au = attr_allocunit(pctx, mach, (int *)&scattr.units);
        if (!saw_ext) saw_ext = attr_extension(pctx, mach, &scattr.signext);
        if (!saw_align) saw_align = attr_align(pctx, mach, &align);
        if (!saw_psect) saw_psect = attr_psect(pctx, stg, &psect);
        if (!saw_init) {
            if (attr_initial(pctx, stg, scattr_units(&scattr), mach, &ivlist)) {
                saw_init = 1;
                if (saw_au == 0) saw_au = -1; // INITIAL must come after allocation-unit
            }
        }
        if (!saw_vol) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_VOLATILE, 0, 1)) {
                saw_vol = 1;
                saw_alias = -1;
                seg_flags_set(stg, seg, SEG_M_VOLATILE);
                nameinfo_data_flags_set(ni, NI_M_VOLATILE);
            }
        }
        if (!saw_alias) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIAS, 0, 1)) {
                saw_vol = -1;
                saw_alias = 1;
                seg_flags_set(stg, seg, SEG_M_ALIAS);
                nameinfo_data_flags_set(ni, NI_M_ALIAS);
            }
        }
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);

    } while (which < 0);

    if (saw_psect > 0) seg_static_psect_set(stg, seg, psect);
    if (saw_au > 0) seg_size_set(stg, seg, scattr.units);
    if (saw_align > 0) seg_alignment_set(stg, seg, align);
    if (saw_init > 0) seg_initval_set(stg, seg, ivlist);
    nameinfo_data_seg_set(ni, seg);
    nameinfo_data_scattr_set(ni, &scattr);

    return which;

} /* handle_static_attrs */

static int
declare_static (parse_ctx_t pctx, scopectx_t scope, stgctx_t stg,
                machinedef_t *mach, decltype_t dt)
{
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    psect_t *psect;
    seg_t *seg;
    int status = 1;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };
    static nametype_t types[4] = { NAMETYPE_OWN, NAMETYPE_GLOBAL,
        NAMETYPE_EXTERNAL, NAMETYPE_FORWARD };

    if (dt == DCL_NORMAL) {
        np = scope_sclass_psectname(scope, SCLASS_OWN);
        psect = name_data_ptr(np);
    } else if (dt == DCL_GLOBAL) {
        np = scope_sclass_psectname(scope, SCLASS_GLOBAL);
        psect = name_data_ptr(np);
    } else {
        psect = 0;
    }
    while (status != 0) {
        nameinfo_t *ni;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        ni = nameinfo_alloc(types[dt]);
        seg = seg_alloc_static(stg, pos, psect);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_static_attrs(pctx, scope, stg, mach, dt, seg, ni);
        } else {
            status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        }
        if (status < 0) {
            nameinfo_free(ni, stg);
            string_free(namestr);
            /* XXX error condition */
            break;
        }
        np = name_declare(scope, namestr->ptr, namestr->len, LEXTYPE_NAME_DATA, pos);
        if (np == 0) {
            /* XXX error condition */
        } else {
            seg_commit(stg, seg);
            name_data_set_ptr(np, ni);
        }
        string_free(namestr);
    }

    return 1;
}

static int
declare_stackseg (parse_ctx_t pctx, scopectx_t scope, stgctx_t stg,
                  machinedef_t *mach, lextype_t lt)
{
    return 1;
}
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
void
declarations_init (scopectx_t kwdscope, stgctx_t stg, machinedef_t *mach)
{
    int i;
    for (i = 0; i < sizeof(decl_names)/sizeof(decl_names[0]); i++) {
        name_insert(kwdscope, &decl_names[i]);
    }

    lextype_register(LEXTYPE_NAME_COMPILETIME, bind_compiletime);
    lextype_register(LEXTYPE_NAME_LITERAL, bind_literal);
    macros_init(kwdscope);
    psects_init(kwdscope, stg, mach);

} /* declarations_init */

/*
 * parse_declaration
 */
int
parse_declaration (parse_ctx_t pctx)
{
    lextype_t lt;
    scopectx_t scope = parser_scope_get(pctx);
    int status;
    stgctx_t stg = parser_get_cctx(pctx);
    machinedef_t *mach = parser_get_machinedef(pctx);
    static lextype_t pfx[] = { LEXTYPE_DCL_GLOBAL, LEXTYPE_DCL_EXTERNAL,
        LEXTYPE_DCL_FORWARD };
    decltype_t  dtypes[] = { DCL_NORMAL, DCL_GLOBAL, DCL_EXTERNAL, DCL_FORWARD };
    int which;
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
        case LEXTYPE_DCL_GLOBAL:
        case LEXTYPE_DCL_OWN:
        case LEXTYPE_DCL_EXTERNAL:
            status = declare_static(pctx, scope, stg, mach, dtypes[which]);
            break;
        case LEXTYPE_DCL_STACKLOCAL:
        case LEXTYPE_DCL_LOCAL:
            status = declare_stackseg(pctx, scope, stg, mach, lt);
            break;
        case LEXTYPE_DCL_MACRO:
        case LEXTYPE_DCL_KEYWORDMACRO:
            status = declare_macro(pctx, scope, lt);
            break;
        case LEXTYPE_DCL_COMPILETIME:
            status = declare_compiletime(pctx, scope, lt);
            break;
        case LEXTYPE_DCL_LITERAL:
            status = declare_literal(pctx, scope, stg, mach, dtypes[which]);
            break;
        case LEXTYPE_DCL_LABEL:
            status = declare_label(pctx, scope);
            break;
        case LEXTYPE_DCL_PSECT:
            status = declare_psect(pctx, scope);
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
                      lexeme_textpos_get(lex));
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

    name_data_set_ptr(np, blkexp);

    return 1;

} /* declare_module */