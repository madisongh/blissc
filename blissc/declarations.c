//
//  declarations.c
//  blissc
//
//  Created by Matthew Madison on 11/11/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "symbols.h"
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

static namedef_t decl_names[] = {
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
    lexctx_t lctx = expr_lexmemctx(ctx);
    lexeme_t *lex, *nlex;
    lextype_t lt;
    name_t *np;

    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &nlex, 0)) {
            /* XXX error condition */
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
            /* XXX error condition */
            lexeme_free(lctx, nlex);
            return 0;
        } else {

            if (!expr_parse_ctce(ctx, &lex)) {
                /* XXX error condition */
                lexeme_free(lctx, nlex);
                return 0;
            } else {
                np = compiletime_declare(scope, lexeme_text(nlex),
                                         lexeme_signedval(lex),
                                         lexeme_textpos_get(nlex));
                if (np == 0) {
                    /* XXX error condition */
                }
                lexeme_free(lctx, lex);
                lexeme_free(lctx, nlex);
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

    if (lt == LEXTYPE_NAME || lexeme_boundtype(lex) == LEXTYPE_NAME) {
        *result = string_copy(0, lexeme_text(lex));
        *pos = lexeme_textpos_get(lex);
        lexeme_free(parser_lexmemctx(pctx), lex);
        return 1;
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
    lexctx_t lctx = expr_lexmemctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    textpos_t pos;
    strdesc_t *namestr;
    literal_attr_t attr;

    while (1) {
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }

        memset(&attr, 0, sizeof(attr));
        attr.width = machine_scalar_bits(mach);

        if (decltype == DCL_EXTERNAL) {
            attr.flags |= SYM_M_NOVALUE;
        } else {
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
                    attr.value = lexeme_signedval(lex);
                    lexeme_free(lctx, lex);
                }
            }

        }

        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_COLON) {
            lt = parser_next(pctx, QL_NORMAL, 0);
            if (lt == LEXTYPE_ATTR_SIGNED || lt == LEXTYPE_ATTR_UNSIGNED) {
                long rval;
                attr.flags |= (lt == LEXTYPE_ATTR_SIGNED) ? SYM_M_SIGNEXT : 0;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    /* XXX error condition */
                }
                if (!expr_parse_ctce(ctx, &lex)) {
                    /* XXX error condition */
                    rval = machine_scalar_bits(mach);
                } else {
                    rval = lexeme_signedval(lex);
                    lexeme_free(lctx, lex);
                }
                if (!machine_signext_supported(mach)) {
                    /* XXX error condition */
                } else if (rval < 1 || rval > machine_scalar_bits(mach)) {
                    /* XXX error condition */
                } else {
                    attr.width = (unsigned int) rval;
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

        np = litsym_declare(scope, namestr,
                            (decltype == DCL_GLOBAL ? SYMSCOPE_GLOBAL
                             : (decltype == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
                                : SYMSCOPE_LOCAL)), &attr, pos);
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
        np = label_declare(scope, namestr, pos);
        if (np == 0) {
            /* XXX error condition */
        }
        string_free(namestr);
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
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    name_t *np;
    int which, has_attr;
    unsigned int attr;
    textpos_t defpos;
    strdesc_t *psname;
    psect_t *ps;
    static lextype_t classkws[] = { LEXTYPE_KWD_NODEFAULT,
        LEXTYPE_DCL_OWN, LEXTYPE_DCL_GLOBAL,
        LEXTYPE_KWD_PLIT, LEXTYPE_KWD_CODE };
    static storageclass_t classes[] = { 0, SCLASS_OWN, SCLASS_GLOBAL,
        SCLASS_PLIT, SCLASS_CODE };


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
        if (!parse_decl_name(pctx, scope, &psname, &defpos)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        has_attr = psect_attr(pctx, &attr);
        if (has_attr < 0) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        ps = psect_search(scope, psname);
        if (ps != 0 && has_attr) {
            unsigned int actflags = psect_flags(ps);
            if (actflags != attr) {
                /* XXX error condition */
            }
        }
        if (ps == 0) {
            np = psect_declare(scope, psname, attr, defpos);
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
        string_free(psname);
    }

    return 1;

} /* declare_psect */

static int
attr_psect (expr_ctx_t ctx, name_t **psnp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    lexeme_t *lex;
    int status = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_PSECT, 0, 1)) {
        return 0;
    }
    parser_punctclass_set(pctx, PUNCT_COMMASEP_PARENS, LEXTYPE_DELIM_COMMA);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_PSECT, &lex, 1)) {
        *psnp = lexeme_ctx_get(lex);
        lexeme_free(lctx, lex);
        status = 1;
    } else {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }

    return status;

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
    lexctx_t lctx = expr_lexmemctx(ctx);
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
                lexeme_free(lctx, lex);
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
                /* XXX error condition */
            }
            parser_punctclass_set(pctx, PUNCT_COMMASEP_PARENS, LEXTYPE_DELIM_COMMA);
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
                lexeme_free(lctx, lex);
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
name_t *
define_plit (expr_ctx_t ctx, lextype_t curlt, textpos_t pos)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    int plitau;
    seg_t *seg;
    name_t *psname, *np;
    initval_t *ivlist;

    if (!attr_psect(ctx, &psname)) {
        psname = scope_sclass_psectname(parser_scope_get(pctx), SCLASS_PLIT);
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

    np = 0;
    seg = seg_alloc_static(stg, parser_curpos(pctx), psect_pointer(psname));
    if (!seg_initval_set(stg, seg, ivlist)) {
        /* XXX error conditon */
        seg_free(stg, seg);
    } else {
        strdesc_t *plitname = tempname_get(expr_namectx(ctx));
        data_attr_t attr;
        memset(&attr, 0, sizeof(attr));
        attr.psect = psname;
        np = datasym_declare(parser_scope_get(pctx), plitname,
                             SYMSCOPE_LOCAL, &attr, pos);
        if (np == 0) {
            /* XXX error condition */
            seg_free(stg, seg);
        } else {
            seg_commit(stg, seg);
            datasym_seg_set(np, seg);
        }
    }

    return np;
    
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
    lexctx_t lctx = expr_lexmemctx(ctx);
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
    lexeme_free(lctx, lex);
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
attr_field (expr_ctx_t ctx, scopectx_t scope, namereflist_t *fldset)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    lexeme_t *lex;
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
        if (parser_expect_oneof(pctx, QL_NORMAL, ftypes, 2, &lex, 1) < 0) {
            /* XXX error condition */
        } else {
            np = lexeme_ctx_get(lex);
            if (lexeme_type(lex) == LEXTYPE_NAME_FIELD) {
                namereflist_instail(fldset, nameref_alloc(namectx, np));
            } else {
                namereflist_append(fldset, fieldset_reflist(np));
            }
            lexeme_free(lctx, lex);
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
attr_preset (expr_ctx_t ctx, name_t *np, seg_t *seg,
             data_attr_t *attr, initval_t **ivlistp)
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
        lex = name_to_lexeme(parser_lexmemctx(pctx), np, parser_curpos(pctx));
        pexp = structure_reference(ctx, attr->struc, 1, np, lex);
        if (pexp == 0) {
            /* XXX error condition */
        }
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
                   seg_t *seg, data_attr_t *attr, name_t *np, int is_bind) {
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    int saw_au, saw_ext, saw_align, saw_init, saw_vol,
        saw_alias, saw_psect, saw_stru, saw_field, saw_preset;
    int did1;
    int align;
    initval_t *ivlist;

    saw_au = saw_ext = saw_align = saw_init = saw_vol = saw_alias = saw_psect = 0;
    saw_stru = 0;
    saw_field = saw_preset = -1; // not allowed until saw_stru

    attr->units = machine_scalar_units(mach);

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
    parser_set_indecl(pctx, 1);

    do {
        did1 = 0;
        if (!saw_au) {
            saw_au = attr_allocunit(ctx, (int *)&attr->units);
            if (saw_au) { did1 = 1; saw_stru = -1; }
        }
        if (!saw_ext) {
            int signext;
            saw_ext = attr_extension(ctx, &signext);
            if (saw_ext > 0) {
                did1 = 1; saw_stru = -1;
                if (signext) attr->flags |= SYM_M_SIGNEXT;
            }
        }
        if (!saw_stru) {
            lexeme_t *lex;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REF, 0, 1)) {
                attr->flags |= SYM_M_REF;
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
                attr->struc = lexeme_ctx_get(lex);
                structure_allocate(ctx, attr->struc, 0,
                                   &attr->units, &attr->struscope);
                saw_au = saw_ext = -1;
                if (saw_preset == -1) saw_preset = 0;
                if (saw_field == -1) saw_field = 0;
                did1 = 1;
            }
        }
        if (!saw_field) {
            saw_field = attr_field(ctx, scope, &attr->fields);
            if (saw_field) {
                did1 = 1;
            }
        }
        if (!saw_align) {
            saw_align = attr_align(ctx, &align);
            if (saw_align) did1 = saw_align;
        }
        if (!saw_psect) {
            saw_psect = attr_psect(ctx, &attr->psect);
            if (saw_psect) { did1 = 1; }
        }
        if (!saw_init) {
            if (attr_initial(ctx, attr->units, &ivlist)) {
                saw_init = 1;
                saw_preset = -1;
                did1 = 1;
                if (saw_au == 0) saw_au = -1; // INITIAL must come after allocation-unit
            }
        }
        if (!saw_preset) {
            datasym_attr_update(np, attr);
            if (attr_preset(ctx, np, seg, attr, &ivlist)) {
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
                attr->flags |= SYM_M_VOLATILE;
            }
        }
        if (!saw_alias) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIAS, 0, 1)) {
                did1 = 1;
                saw_vol = -1;
                saw_alias = 1;
                seg_flags_set(stg, seg, SEG_M_ALIAS);
                attr->flags |= SYM_M_ALIAS;
            }
        }

    } while (did1);

    parser_set_indecl(pctx, 0);

    // XXX need to compare attributes for FORWARD/EXTERNAL against
    //     their actual OWN/GLOBALs
    
    if (saw_psect > 0) seg_static_psect_set(stg, seg,
                                            psect_pointer(attr->psect));
    if (saw_align > 0) seg_alignment_set(stg, seg, align);
    if (saw_init > 0) seg_initval_set(stg, seg, ivlist);
    if (saw_preset > 0) seg_preset_set(stg, seg, ivlist);


    if (saw_stru > 0) {
        if (seg != 0) {
            if (attr->flags & SYM_M_REF) {
                seg_size_set(stg, seg, machine_scalar_units(mach));
            } else {
                seg_size_set(stg, seg, attr->units);
            }
        }
    } else {
        if (saw_field > 0) {
            /* XXX error condition */
            namereflist_free(expr_namectx(ctx), &attr->fields);
        }
        if (saw_au > 0 && seg != 0) seg_size_set(stg, seg, attr->units);
    }

    return 1;

} /* handle_data_attrs */

static int
declare_data (expr_ctx_t ctx, scopectx_t scope, lextype_t lt, decltype_t dt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    stgctx_t stg = expr_stg_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np, *psname;
    psect_t *psect = 0;
    segtype_t st;
    int status = 1;
    int stackonly = 0;
    static symscope_t sc;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };


    psname = 0;
    switch (lt) {
        case LEXTYPE_DCL_STACKLOCAL:
            stackonly = 1;
            // FALLTHROUGH
        case LEXTYPE_DCL_LOCAL:
            if (expr_current_routine(ctx) == 0) {
                /* XXX error condition */
            }
            st = SEGTYPE_STACK;
            break;
        case LEXTYPE_DCL_REGISTER:
            if (expr_current_routine(ctx) == 0) {
                /* XXX error condition */
            }
            st = SEGTYPE_REGISTER;
            break;
        default:
            if (dt == DCL_NORMAL) {
                psname = scope_sclass_psectname(scope, SCLASS_OWN);
                psect = psect_pointer(psname);
            } else if (dt == DCL_GLOBAL) {
                psname = scope_sclass_psectname(scope, SCLASS_GLOBAL);
                psect = psect_pointer(psname);
            }
            st = SEGTYPE_STATIC;
            break;
    }

    sc = (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL
          : (dt == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
             : SYMSCOPE_LOCAL));

    while (status != 0) {
        data_attr_t attr;
        seg_t *seg;

        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }

        if (st == SEGTYPE_STACK) {
            seg = seg_alloc_stack(stg, pos, stackonly);
        } else if (st == SEGTYPE_REGISTER) {
            seg = seg_alloc_register(stg, pos);
        } else {
            seg = seg_alloc_static(stg, pos, psect);
        }
        memset(&attr, 0, sizeof(attr));
        if (stackonly) attr.flags |= SYM_M_STACKONLY;
        attr.psect = psname;
        attr.flags |= SYM_M_PENDING;
        np = datasym_declare(scope, namestr, sc, &attr, pos);
        datasym_seg_set(np, seg);

        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, seg, &attr, np, 0);
            if (!status) {
                /* XXX error condition */
            }
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status >= 0) {
            attr.flags &= ~SYM_M_PENDING;
            if (!datasym_attr_update(np, &attr)) {
                /* XXX error condition */
                status = -1;
            }
        }
        if (status < 0) {
            seg_free(stg, seg);
            string_free(namestr);
            datasym_seg_set(np, 0);
            name_undeclare(scope, np);
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
        data_attr_t attr;
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
        memset(&attr, 0, sizeof(attr));
        attr.flags = SYM_M_PENDING;
        // Allocate storage for the BIND pointer.  If we're bound
        // to an LTCE, make it static storage (with a global symbol
        // if it's a GLOBAL BIND).  Otherwise, it's local storage.
        if (expr_is_ltce(exp)) {
            psect_t *psect = 0;
            if (dt != DCL_GLOBAL) {
                attr.psect = scope_sclass_psectname(scope, SCLASS_PLIT);
                psect = psect_pointer(attr.psect);
            }
            seg = seg_alloc_static(stg, pos, psect);
        } else {
            seg = seg_alloc_stack(stg, pos, 0);
        }
        ivlist = expr_initval_add(ctx, 0, exp, machine_scalar_units(mach));
        seg_initval_set(stg, seg, ivlist);

        np = datasym_declare(scope, namestr,
                             (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL : SYMSCOPE_LOCAL),
                             &attr, pos);
        if (np == 0) {
            /* XXX error condition */
        }

        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, seg, &attr, np, 1);
            if (!status) {
                /* XXX error condition */
            }
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status < 0) {
            seg_free(stg, seg);
            string_free(namestr);
            if (np != 0) name_undeclare(scope, np);
            /* XXX error condition */
            break;
        }
        seg_commit(stg, seg);
        attr.flags &= ~SYM_M_PENDING;
        datasym_attr_update(np, &attr);
        datasym_seg_set(np, seg);
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
        data_attr_t attr;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        np = datasym_search(scope, namestr, &attr);
        if (np == 0) {
            /* XXX error condition */
            return 0;
        }
        if (name_scope(np) != scope) {
            attr.flags |= SYM_M_PENDING;
            np = datasym_declare(scope, namestr, SYMSCOPE_LOCAL, &attr, pos);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, DCL_MAP,
                                       datasym_seg(np), &attr, np, 0);
            if (!status) {
                /* XXX error condition */
            } else {
                attr.flags &= ~SYM_M_PENDING;
                datasym_attr_update(np, &attr);
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
               scopectx_t *argtable, namereflist_t *inargs,
               namereflist_t *outargs)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    namereflist_t *curargs;
    name_t *np;
    int which;
    static lextype_t delims[3] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_SEMI,
                                   LEXTYPE_DELIM_COMMA };


    namereflist_init(inargs);
    namereflist_init(outargs);
    curargs = inargs;

    curscope = parser_scope_begin(pctx);

    while (1) {
        data_attr_t attr;
        memset(&attr, 0, sizeof(attr));
        attr.flags = SYM_M_PENDING;
        if (parse_decl_name(pctx, curscope, &namestr, &pos)) {
            if (*argtable == 0) {
                *argtable = scope_begin(expr_namectx(ctx), 0);
            }
            np = datasym_declare(*argtable, namestr, SYMSCOPE_LOCAL, &attr, pos);
            string_free(namestr);
            if (np == 0) {
                /* XXX error condition */
                break;
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                if (!handle_data_attrs(ctx, curscope, DCL_MAP, 0, &attr, np, 0)) {
                    /* XXX error condition */
                }
            }
            attr.flags &= ~SYM_M_PENDING;
            datasym_attr_update(np, &attr);
        } else {
            np = 0;
        }
        namereflist_instail(curargs, nameref_alloc(namectx, np));
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 3, 0, 1);
        if (which == 1) { // semicolon
            curargs = outargs;
            continue;
        }
        if (which != 2) {
            break;
        }
    }
    parser_scope_end(pctx);

    return (which == 0);
    
} /* parse_formals */

static int
handle_routine_attrs (expr_ctx_t ctx, scopectx_t scope,
                      decltype_t dt, seg_t *seg, routine_attr_t *attr,
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

    do {
        did1 = 0;
        if (!saw_psect) {
            name_t *psname;
            saw_psect = attr_psect(ctx, &psname);
            if (saw_psect) {
                did1 = 1;
                psect = psect_pointer(psname);
            }
        }
        if (!saw_novalue) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_NOVALUE, 0, 1)) {
                did1 = 1;
                saw_novalue = 1;
                attr->flags |= SYM_M_NOVALUE;
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
    strdesc_t *namestr;
    textpos_t pos;
    scopectx_t argscope;
    psect_t *psect = 0;
    seg_t *seg;
    name_t *np;
    symscope_t sc;
    int which;
    static lextype_t delims[4] = { LEXTYPE_DELIM_SEMI,
        LEXTYPE_DELIM_COMMA, LEXTYPE_OP_ASSIGN, LEXTYPE_DELIM_COLON };


    if (!is_bind) {
        np = scope_sclass_psectname(scope, SCLASS_CODE);
        psect = psect_pointer(np);
    } else if (dt == DCL_GLOBAL) {
        np = scope_sclass_psectname(scope, SCLASS_GLOBAL);
        psect = psect_pointer(np);
    }

    sc = (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL
          : (dt == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
             : SYMSCOPE_LOCAL));

    while (1) {
        routine_attr_t attr;
        argscope = 0;
        if (!parse_decl_name(pctx, scope, &namestr, &pos)) {
            /* XXX error condition */
            return 0;
        }
        memset(&attr, 0, sizeof(attr));
        if (is_bind) {
            if (dt == DCL_GLOBAL) {
                seg = seg_alloc_static(stg, pos, psect);
            } else {
                seg = seg_alloc_stack(stg, pos, 0);
            }
        } else {
            seg = seg_alloc_static(stg, pos, psect);
        }

        if (dt != DCL_EXTERNAL) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                if (!parse_formals(ctx, scope, &attr.argscope,
                                   &attr.inargs, &attr.outargs)) {
                    /* XXX error condition */
                    if (seg != 0) seg_free(stg, seg);
                    return 0;
                }
            }
        }
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 4, 0, 1);
        if (which == 3) {  // the colon
            which = handle_routine_attrs(ctx, scope, dt, seg, &attr, is_bind);
            if (which < 0) {
                /* XXX error condition */
                if (seg != 0) seg_free(stg, seg);
                return 0;
            }
        }
        attr.flags |= SYM_M_PENDING;
        np = rtnsym_declare(scope, namestr, sc, &attr, pos);
        if (is_bind || dt == DCL_NORMAL || dt == DCL_GLOBAL) {
            expr_node_t *exp;

            if (which != 2) { // i.e., the '='
                /* XXX error condition - need expression */
                if (seg != 0) seg_free(stg, seg);
                return 0;
            }
            if (!is_bind) {
                expr_push_routine(ctx, np);
                // XXX Temporary until we are properly
                //     handling linkages XXX
                nameref_t *arg;
                for (arg = namereflist_head(&attr.inargs); arg != 0;
                     arg = arg->tq_next) {
                    if (arg->np != 0) {
                        seg_t *aseg = seg_alloc_stack(stg, pos, 0);
                        seg_size_set(stg, aseg, machine_scalar_units(mach));
                        seg_commit(stg, aseg);
                        datasym_seg_set(arg->np, aseg);
                    }
                }
                if (attr.argscope != 0) {
                    argscope = scope_copy(attr.argscope, 0);
                    parser_scope_set(pctx, argscope);
                }
            }
            if (!expr_expr_next(ctx, &exp)) {
                /* XXX error condition */
            }
            if (!expr_has_value(exp)) {
                /* XXX error condition */
            }
            if (is_bind) {
                initval_t *ivlist;
                ivlist = initval_expr_add(stg, 0, 1, 1, exp, 32, 0);
                seg_initval_set(stg, seg, ivlist);
            } else {
                if (argscope != 0) parser_scope_end(pctx);
                frame_end(stg);
                rtnsym_expr_set(np, exp);
            }
        }
        if (seg != 0) seg_commit(stg, seg);
        attr.flags &= ~SYM_M_PENDING;
        rtnsym_attr_update(np, &attr);
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
    lexeme_free(parser_lexmemctx(pctx), lex);
    return 1;

} /* declare_require */

/*
 * undeclare
 */
static int
undeclare (parse_ctx_t pctx, scopectx_t scope)
{
    lexeme_t *lex;

    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &lex, 1)) {
            /* XXX error condition */
        } else {
            if (!sym_undeclare(scope, lexeme_text(lex))) {
                /* XXX error condition */
            }
        }
        lexeme_free(parser_lexmemctx(pctx), lex);
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
 * psects_init
 *
 * Set up the default PSECTs and their predeclared names.
 * XXX this should probably be machine-specific.
 */
static void
psects_init (scopectx_t kwdscope, stgctx_t stg, machinedef_t *mach) {

    name_t *np;
    strdesc_t own = STRDEF("$OWN$");
    strdesc_t global = STRDEF("$GLOBAL$");
    strdesc_t plit = STRDEF("$PLIT$");
    strdesc_t code = STRDEF("$CODE$");

    np = psect_declare(kwdscope, &own, PSECT_M_ATTR_WRITE, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_OWN, np);
    np = psect_declare(kwdscope, &global, PSECT_M_ATTR_WRITE, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_GLOBAL, np);
    np = psect_declare(kwdscope, &plit, 0, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_PLIT, np);
    np = psect_declare(kwdscope, &code, PSECT_M_ATTR_EXEC, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_CODE, np);

} /* psects_init */

/*
 * declarations_init
 *
 * Initialization routine.
 */
void
declarations_init (expr_ctx_t ctx, parse_ctx_t pctx,
                   scopectx_t kwdscope,
                   stgctx_t stg, machinedef_t *mach)
{
    int i;
    literal_attr_t attr;
    static strdesc_t bpdsc[4] = {
        STRDEF("%BPUNIT"), STRDEF("%BPADDR"), STRDEF("%BPVAL"),
        STRDEF("%UPVAL") };

    for (i = 0; i < sizeof(decl_names)/sizeof(decl_names[0]); i++) {
        name_declare(kwdscope, &decl_names[i], 0, 0, 0, 0);
    }

    symbols_init(ctx);
    macros_init(kwdscope, ctx);
    psects_init(kwdscope, stg, mach);

    attr.width = machine_unit_bits(mach);
    attr.flags = SYM_M_RESERVED;
    attr.value = machine_unit_bits(mach);
    litsym_declare(kwdscope, &bpdsc[0], SYMSCOPE_LOCAL, &attr, 0);
    attr.value = machine_addr_bits(mach);
    litsym_declare(kwdscope, &bpdsc[1], SYMSCOPE_LOCAL, &attr, 0);
    attr.value = machine_scalar_bits(mach);
    litsym_declare(kwdscope, &bpdsc[2], SYMSCOPE_LOCAL, &attr, 0);
    attr.value = machine_scalar_units(mach);
    litsym_declare(kwdscope, &bpdsc[3], SYMSCOPE_LOCAL, &attr, 0);
    structures_init(ctx);
 
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

    parser_punctclass_set(pctx, PUNCT_COMMASEP_NOGROUP, LEXTYPE_DELIM_COMMA);
    
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
            status = declare_data(ctx, scope, lt, dtypes[which]);
            break;
        case LEXTYPE_DCL_STACKLOCAL:
        case LEXTYPE_DCL_LOCAL:
            status = declare_data(ctx, scope, lt, DCL_NORMAL);
            break;
        case LEXTYPE_DCL_REGISTER:
            status = declare_data(ctx, scope, lt, dtypes[which]);
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
    strdesc_t *text;
    expr_node_t *blkexp;
    textpos_t pos;
    name_t *np;
    namedef_t ndef;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_MODULE, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    if (!parse_decl_name(pctx, scope, &text, &pos)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MODULE;
    ndef.flags = 0;
    ndef.name = text->ptr;
    ndef.namelen = text->len;
    np = name_declare(scope, &ndef, pos, 0, 0, 0);
    string_free(text);
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

    name_value_pointer_set(np, blkexp);

    return 1;

} /* declare_module */