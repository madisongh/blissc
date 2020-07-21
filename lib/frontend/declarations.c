/*
 *++
 * declarations.c - Data, routine, module declarations
 *
 * This module implements the parsing and binding of data
 * and routine names, as well as declarations for
 * labels, psects, and modules.
 *
 * COMPILETIME names are simple signed integers; the value
 * is stored directly in the name_t cell.
 *
 * The MODULE name has attributes, but is relatively easy
 * to track, since there is only one of these per module.
 *
 * All other names have attributes and run-time scope
 * that needs to be tracked; this module handles parsing of
 * the various attributes and calls on the symbols module
 * to manage the names.
 *
 * Copyright © 2012, 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include "blissc/symbols.h"
#include "blissc/gencode.h"
#include "blissc/declarations.h"
#include "blissc/macros.h"
#include "blissc/switches.h"

struct declctx_s {
    macroctx_t mctx;
};

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
    NAMEDEF("LIBRARY", LEXTYPE_DCL_LIBRARY, NAME_M_RESERVED),
    NAMEDEF("SWITCHES", LEXTYPE_DCL_SWITCHES, NAME_M_RESERVED),
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
    NAMEDEF("NOVALUE", LEXTYPE_ATTR_NOVALUE, NAME_M_RESERVED),
    NAMEDEF("%ASSIGN", LEXTYPE_LXF_ASSIGN, NAME_M_RESERVED),
    NAMEDEF("REP", LEXTYPE_KWD_REP, NAME_M_RESERVED)
};

static int parse_ASSIGN(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);

/*
 * toggle_errs
 *
 * Handler for the ERRS switch, to enable/disable error logging to the terminal.
 */
static int
toggle_errs (parse_ctx_t pctx, void *ctx, int togidx, lextype_t dcltype, name_t *togname)
{
    log_logterm_set(parser_logctx(pctx), name_type(togname) != LEXTYPE_NAME_SW_TOGGLE_OFF);
    return 1;

} /* toggle_errs */

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
    strctx_t strctx = expr_strctx(ctx);
    textpos_t pos;
    lextype_t lt;
    name_t *np;

    while (1) {
        strdesc_t *namestr;
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
            expr_signal(ctx, STC__OPEREXP, "=");
        } else {
            long val;
            if (!expr_parse_ctce(ctx, 0, &val)) {
                expr_signal(ctx, STC__EXPCTCE);
            } else {
                np = compiletime_declare(scope, namestr, val, pos);
                if (np == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "declare_compiletime");
                }
            }
            string_free(strctx, namestr);
        }

        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            expr_signal(ctx, STC__DELIMEXP, ",");
            string_free(expr_strctx(ctx), namestr);
            return 0;
        }

    } /* while 1 */

    return 1;

} /* declare_compiletime */


/*
 * parse_decl_name
 *
 * Common parsing logic for names to be declared
 * (quote-level QL_NAME).
 */
int
parse_decl_name (parse_ctx_t pctx, strdesc_t **result, textpos_t *pos)
{
    lextype_t lt;
    lexeme_t *lex;

    lt = parser_next(pctx, QL_NAME, &lex);

    if (lt == LEXTYPE_NAME || lexeme_boundtype(lex) == LEXTYPE_NAME) {
        strdesc_t *ltext = lexeme_text(lex);
        if (ltext->len > NAME_SIZE) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx),
                       STC__NAMETOOLON, ltext, NAME_SIZE-1);
            *result = string_from_chrs(parser_strctx(pctx), 0, ltext->ptr, NAME_SIZE-1);
        } else {
            *result = string_copy(parser_strctx(pctx), 0, ltext);
        }
        *pos = parser_curpos(pctx);
        lexeme_free(parser_lexmemctx(pctx), lex);
        return 1;
    }

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
    machinedef_t *mach = expr_machinedef(ctx);
    lextype_t lt;
    name_t *np;
    textpos_t pos;
    strdesc_t *namestr;
    literal_attr_t attr;

    while (1) {
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }

        memset(&attr, 0, sizeof(attr));
        attr.width = machine_scalar_bits(mach);

        if (decltype == DCL_EXTERNAL) {
            attr.flags |= SYM_M_NOVALUE;
        } else {
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
                expr_signal(ctx, STC__OPEREXP, "=");
                string_free(expr_strctx(ctx), namestr);
                return 0;
            } else {
                long val;
                if (!expr_parse_ctce(ctx, 0, &val)) {
                    expr_signal(ctx, STC__EXPCTCE);
                    string_free(expr_strctx(ctx), namestr);
                    return 0;
                }
                attr.value = (unsigned long) val;
            }
        }

        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_COLON) {
            lt = parser_next(pctx, QL_NORMAL, 0);
            if (lt == LEXTYPE_ATTR_SIGNED || lt == LEXTYPE_ATTR_UNSIGNED) {
                long rval;
                attr.flags |= (lt == LEXTYPE_ATTR_SIGNED) ? SYM_M_SIGNEXT : 0;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    expr_signal(ctx, STC__DELIMEXP, "(");
                }
                if (!expr_parse_ctce(ctx, 0, &rval)) {
                    expr_signal(ctx, STC__EXPCTCE);
                    rval = machine_scalar_bits(mach);
                }
                if (!machine_signext_supported(mach)) {
                    expr_signal(ctx, STC__SGNEXTUNS);
                } else if (rval < 1 || rval > machine_scalar_bits(mach)) {
                    expr_signal(ctx, STC__LITRNGERR, rval);
                } else {
                    attr.width = (unsigned int) rval;
                }
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                    expr_signal(ctx, STC__DELIMEXP, ")");
                    string_free(expr_strctx(ctx), namestr);
                    return 0;
                }
            }
            // now get the semicolon or comma, for later
            lt = parser_next(pctx, QL_NORMAL, 0);
        }

        attr.sc = (decltype == DCL_GLOBAL ? SYMSCOPE_GLOBAL
                   : (decltype == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
                      : SYMSCOPE_LOCAL));
        np = litsym_declare(scope, namestr, &attr, pos);
        string_free(expr_strctx(ctx), namestr);
        if (np == 0) {
            expr_signal(ctx, STC__INTCMPERR, "declare_literal");
        }

        if (lt == LEXTYPE_DELIM_SEMI) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            expr_signal(ctx, STC__DELIMEXP, ",");
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
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__NAMEEXP);
            return 0;
        }
        np = label_declare(scope, namestr, pos);
        if (np == 0) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx),
                       STC__INTCMPERR, "declare_label");
        }
        string_free(parser_strctx(pctx), namestr);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx),
                       STC__DELIMEXP, ",");
        }
    }
    return 1;

} /* declare_label */

/*
 * psect_attr
 *
 * Handle the common attributes for a psect.
 */
static int
psect_parse_attr (parse_ctx_t pctx, unsigned int *attrp)
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
            log_signal(parser_logctx(pctx), parser_curpos(pctx),
                       STC__DELIMEXP, ",");
        }
    }

    *attrp = attr;
    return 1;
}

/*
 * declare_psect
 *
 * Declare a PSECT.
 *
 * PSECT OWN|GLOBAL|PLIT|CODE|NODEFAULT = name { (attribute,...) }
 */
static int
declare_psect (expr_ctx_t ctx, scopectx_t scope)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    name_t *np;
    int which, has_attr;
    unsigned int attr = 0;
    textpos_t defpos;
    strdesc_t *psname;
    static lextype_t classkws[] = { LEXTYPE_KWD_NODEFAULT,
        LEXTYPE_DCL_OWN, LEXTYPE_DCL_GLOBAL,
        LEXTYPE_KWD_PLIT, LEXTYPE_KWD_CODE };
    static storageclass_t classes[] = { 0, SCLASS_OWN, SCLASS_GLOBAL,
        SCLASS_PLIT, SCLASS_CODE };


    while (1) {
        which = parser_expect_oneof(pctx, QL_NORMAL, classkws,
                                 sizeof(classkws)/sizeof(classkws[0]), 0, 1);
        if (which < 0) {
            expr_signal(ctx, STC__STOCLSEXP);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            expr_signal(ctx, STC__OPEREXP, "=");
        }
        if (!parse_decl_name(pctx, &psname, &defpos)) {
            expr_signal(ctx, STC__NAMEEXP);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        has_attr = psect_parse_attr(pctx, &attr);
        if (has_attr < 0) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            return 0;
        }
        np = psect_search(scope, psname);
        if (np != 0 && has_attr) {
            unsigned int actflags = psect_attr(np);
            if (actflags != attr) {
                expr_signal(ctx, STC__PSATTRMSM, psname);
            }
        }
        if (np == 0) {
            np = psect_declare(scope, psname, attr, defpos);
        }
        if (which > 0) {
            scope_sclass_psectname_set(scope, classes[which], np);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP);
        }
        string_free(expr_strctx(ctx), psname);
    }

    return 1;

} /* declare_psect */

/*
 * attr_psect
 *
 * Parse the PSECT attribute.
 */
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
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_PSECT, &lex, 1)) {
        *psnp = lexeme_ctx_get(lex);
        lexeme_free(lctx, lex);
        status = 1;
    } else {
        expr_signal(ctx, STC__PSNAMEXP);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    return status;

} /* attr_psect */

/*
 * attr_allocunit
 *
 * Parse an allocation-unit attribute.  Only those allocation-unit
 * keywords that are supported for the target machine will be recognized.
 */
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
    if (1U<<which > machine_scalar_maxbytes(mach)) {
        expr_signal(ctx, STC__AUSIZERR, (1<<which));
        *valp = machine_scalar_maxbytes(mach);
    } else {
        *valp = 1 << which;
    }
    return 1;

} /* attr_allocunit */


/*
 * plit_items
 *
 * Parses an initval list for a PLIT, UPLIT, or the INITIAL
 * attribute on a data declaration.
 */
static initval_t *
plit_items (expr_ctx_t ctx, int defau, int is_static) {

    symctx_t symctx = expr_symctx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    initval_t *ivlist, *iv;
    int itemau = defau;
    lexeme_t *lex;
    int ltces_ok = machine_linktime_constant_initializers(expr_machinedef(ctx));
    static lextype_t strtypes[] = { LEXTYPE_STRING, LEXTYPE_CSTRING };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    ivlist = 0;
    while (1) {
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_REP, 0, 1)) {
            long val;
            unsigned int repcount;
            if (!expr_parse_ctce(ctx, 0, &val)) {
                expr_signal(ctx, STC__EXPCTCE);
                repcount = 1;
            } else {
                repcount = (unsigned int) val;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
                expr_signal(ctx, STC__KWDEXP, "OF");
            }
            parser_punctclass_set(pctx, PUNCT_COMMASEP_PARENS, LEXTYPE_DELIM_COMMA);
            if (attr_allocunit(ctx, &itemau) == 0) {
                itemau = defau;
            }
            iv = plit_items(ctx, itemau, is_static);
            if (iv != 0) {
                ivlist = initval_ivlist_add(symctx, ivlist, repcount, iv);
            }
        } else if (attr_allocunit(ctx, &itemau) != 0) {
            iv = plit_items(ctx, itemau, is_static);
            if (iv != 0) {
                ivlist = initval_ivlist_add(symctx, ivlist, 1, iv);
            }
        } else {
            expr_node_t *exp;
            lex = 0;
            if (parser_expect_oneof(pctx, QL_NORMAL, strtypes, 2, &lex, 1) >= 0) {
                ivlist = initval_string_add(symctx, ivlist, 1, lexeme_text(lex));
                lexeme_free(lctx, lex);
            } else if (expr_parse_expr(ctx, &exp)) {
                if (is_static) {
                    if (ltces_ok && !expr_is_ltce(exp)) {
                        expr_signal(ctx, STC__LTCEREQD);
                        expr_node_free(ctx, exp);
                        exp = 0;
                    } else if (!ltces_ok && !expr_is_ctce(exp)) {
                        expr_signal(ctx, STC__EXPCTCE);
                        expr_node_free(ctx, exp);
                        exp = 0;
                    }
                    // If it wasn't valid, substitute zero so we can keep going
                    if (exp == 0) {
                        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT,
                                              parser_curpos(pctx));
                        expr_litval_set(exp, 0);
                    }
                }
                ivlist = expr_initval_add(ctx, ivlist, exp, itemau);
            } else {
                expr_signal(ctx, STC__EXPREXP);
                initval_freelist(symctx, ivlist);
                return 0;
            }
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
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
    symctx_t symctx = expr_symctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    int plitau;
    name_t *psname, *np;
    char namebuf[NAME_SIZE];
    strdesc_t plitname;
    data_attr_t attr;
    initval_t *ivlist;
    unsigned long size;
    unsigned int padding, overage;

    if (!attr_psect(ctx, &psname)) {
        psname = scope_sclass_psectname(parser_scope_get(pctx), SCLASS_PLIT);
    }
    if (!attr_allocunit(ctx, &plitau)) {
        plitau = machine_scalar_units(mach);
    }
    ivlist = plit_items(ctx, plitau, 1);
    if (ivlist == 0) {
        return 0;
    }

    // Must pad out to integral number of fullwords
    size = initval_size(symctx, ivlist);
    overage = (size == 0 ? 0 : (size % machine_scalar_units(mach)));
    padding = (overage == 0 ? 0 : machine_scalar_units(mach) - overage);
    if (padding != 0) {
        ivlist = initval_scalar_add(symctx, ivlist, padding, 0, 1, 0);
    }
    // For counted PLITs, insert the fullword count at the beginning
    if (curlt == LEXTYPE_KWD_PLIT) {
        size = initval_size(symctx, ivlist) / machine_scalar_units(mach);
        ivlist = initval_scalar_prepend(symctx, ivlist, 1, size,
                                        machine_scalar_units(mach), 0);
    }

    np = 0;
    strdesc_init(&plitname, namebuf, 0);
    plitname.len = tempname_get(expr_namectx(ctx), namebuf, sizeof(namebuf));
    memset(&attr, 0, sizeof(attr));
    attr.owner = psname;
    attr.flags = SYM_M_PLIT;
    attr.dclass = DCLASS_STATIC;
    attr.ivlist = ivlist;
    attr.units = (unsigned int) initval_size(symctx, ivlist);
    attr.width = machine_scalar_bits(mach);
    attr.sc = SYMSCOPE_LOCAL;
    np = datasym_declare(parser_scope_get(pctx), &plitname, &attr, pos);
    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "define_plit[2]");
    }

    return np;

} /* define_plit */

/*
 * attr_extension
 *
 * Parse the sign-extension attribute, for those machines that
 * support it.
 */
static int
attr_extension (expr_ctx_t ctx, int *signext)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    int which;
    static lextype_t attrs[] = { LEXTYPE_ATTR_UNSIGNED,
        LEXTYPE_ATTR_SIGNED };

    if (!machine_signext_supported(mach)) {
        return -1;
    }
    which = parser_expect_oneof(pctx, QL_NORMAL, attrs,
                                sizeof(attrs)/sizeof(attrs[0]), 0, 1);
    if (which < 0) {
        return 0;
    }
    *signext = which;
    return 1;

} /* attr_extension */

/*
 * attr_align
 *
 * Parse the ALIGN(n) attribute.
 */
static int
attr_align (expr_ctx_t ctx, unsigned int *valp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    long val;

    if (mach->bpval % 8 != 0) {
        return 0; // not supported with weird word sizes
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIGN, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    if (!expr_parse_ctce(ctx, 0, &val)) {
        expr_signal(ctx, STC__EXPCTCE);
        val = machine_align_max(mach);
    }
    if (val < 0 || val > machine_align_max(mach)) {
        expr_signal(ctx, STC__INVALIGN, val);
        *valp = machine_align_max(mach);
    } else {
        *valp = (unsigned int)val;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }
    return 1;

} /* attr_align */

/*
 * attr_initial
 *
 * Handle the INITIAL attribute.
 */
static int
attr_initial (expr_ctx_t ctx, int defau, int is_static, initval_t **ivlistp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_INITIAL, 0, 1)) {
        return 0;
    }
    *ivlistp = plit_items(ctx, defau, is_static);
    return 1;

} /* attr_initial */

/*
 * attr_field
 *
 * Parse the FIELD attribute, accepting one or more field or field-set names.
 */
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
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 0;
    }
    while (1) {
        if (parser_expect_oneof(pctx, QL_NORMAL, ftypes, 2, &lex, 1) < 0) {
            expr_signal(ctx, STC__FFSNAMEXP);
        } else {
            np = lexeme_ctx_get(lex);
            if (lexeme_type(lex) == LEXTYPE_NAME_FIELD) {
                namereflist_instail(fldset, nameref_alloc(namectx, np));
            } else {
                if (!namereflist_copy(namectx, fldset, fieldset_reflist(np))) {
                    expr_signal(ctx, STC__INTCMPERR, "attr_field");
                }
            }
            lexeme_free(lctx, lex);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    return 1;

} /* attr_field */

/*
 * attr_preset
 *
 * Parse the PRESET attribute.
 */
static int
attr_preset (expr_ctx_t ctx, name_t *np, data_attr_t *attr)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    symctx_t symctx = expr_symctx(ctx);
    lexeme_t *lex;
    initval_t *ivlist = 0;
    expr_node_t *pexp, *exp;
    int ltces_ok = machine_linktime_constant_initializers(expr_machinedef(ctx));

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_PRESET, 0, 1)) {
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    if (np == 0) {
        /* previous error condition - just skip processing */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    while (1) {
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, "[");
        }
        // We're essentially building an assignment expression that
        // will get interpreted at a later stage.
        lex = name_to_lexeme(parser_lexmemctx(pctx), np);
        pexp = structure_reference(ctx, attr->struc, 1, np, lex);
        if (pexp == 0) {
            expr_signal(ctx, STC__INTCMPERR, "attr_preset");
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            expr_signal(ctx, STC__OPEREXP, "=");
        }
        exp = 0;
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
        }
        if (exp != 0) {
            if (attr->dclass == DCLASS_STATIC) {
                if (ltces_ok && !expr_is_ltce(exp)) {
                    expr_signal(ctx, STC__LTCEREQD);
                    expr_node_free(ctx, exp);
                    exp = 0;
                } else if (!ltces_ok && !expr_is_ctce(exp)) {
                    expr_signal(ctx, STC__EXPCTCE);
                    expr_node_free(ctx, exp);
                    exp = 0;
                }
            }
        }
        if (exp != 0) {
            ivlist = preset_expr_add(symctx, ivlist, pexp, exp);
        } else {
            expr_node_free(ctx, pexp);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    } /* while */

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    attr->ivlist = ivlist;

    return 1;

} /* attr_preset */

/*
 * handle_data_attrs
 *
 * Common routine for parsing all attributes on a data declaration.
 * The 'saw_X' variables are used to track which attributes are allowed
 * or seen: if negative, they are not allowed for the type of symbol being
 * declared; if zero, the attribute hasn't been seen yet; if positive, the
 * attribute has been seen.  Some of them get modified as attributes appear,
 * since some attributes preclude others.
 *
 * XXX This is pretty ugly, consider redesigning.
 */
static int
handle_data_attrs (expr_ctx_t ctx, scopectx_t scope, decltype_t dt,
                   data_attr_t *attr, name_t *np) {
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    int saw_au, saw_ext, saw_align, saw_init, saw_vol,
        saw_alias, saw_psect, saw_stru, saw_field, saw_preset;
    int did1;

    saw_au = saw_ext = saw_align = saw_init = saw_vol = saw_alias = saw_psect = 0;
    saw_stru = 0;
    saw_field = saw_preset = -1; // not allowed until saw_stru

    attr->units = machine_scalar_units(mach);

    if ((attr->flags & SYM_M_BIND) != 0 || dt == DCL_MAP) {
        saw_init = saw_psect = saw_align = -1;
        saw_preset = -2;
    } else {
        if (attr->dclass == DCLASS_REGISTER) {
            saw_align = saw_vol = saw_alias = saw_psect = -1;
        } else if (attr->dclass == DCLASS_STKORREG ||
                   attr->dclass == DCLASS_STACKONLY) {
            saw_psect = -1;
        } else {
            if (dt == DCL_FORWARD || dt == DCL_EXTERNAL) {
                saw_align = saw_init = -1;
            }
        }
    }

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
                    expr_signal(ctx, STC__STRUNMEXP);
                }
                saw_stru = 1;
            } else {
                saw_stru = parser_expect(pctx, QL_NORMAL,
                                         LEXTYPE_NAME_STRUCTURE, &lex, 1);
            }
            if (saw_stru) {
                unsigned int nunits;
                attr->struc = lexeme_ctx_get(lex);
                structure_allocate(ctx, attr->struc, 0,
                                   &nunits, &attr->struscope,
                                   (attr->flags & SYM_M_REF) != 0);
                attr->units = nunits;
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
            saw_align = attr_align(ctx, &attr->alignment);
            if (saw_align) did1 = saw_align;
        }
        if (!saw_psect) {
            saw_psect = attr_psect(ctx, &attr->owner);
            if (saw_psect) { did1 = 1; }
        }
        if (!saw_init) {
            unsigned int defau = (attr->struc ? machine_scalar_units(mach)
                                  : attr->units);
            if (attr_initial(ctx, defau, (attr->dclass == DCLASS_STATIC),
                             &attr->ivlist)) {
                saw_init = 1;
                saw_preset = -1;
                did1 = 1;
                if (saw_au == 0) saw_au = -1; // INITIAL must come after allocation-unit
            }
        }
        if (!saw_preset) {
            datasym_attr_update(np, attr);
            if (attr_preset(ctx, np, attr)) {
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
                attr->flags |= SYM_M_VOLATILE;
            }
        }
        if (!saw_alias) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_ATTR_ALIAS, 0, 1)) {
                did1 = 1;
                saw_vol = -1;
                saw_alias = 1;
                attr->flags |= SYM_M_ALIAS;
            }
        }

    } while (did1);

    if (!saw_stru && saw_field > 0) {
        expr_signal(ctx, STC__FLDNOSTRU);
        namereflist_free(expr_namectx(ctx), &attr->fields);
    }
    if (saw_au > 0) {
        attr->width = attr->units * machine_unit_bits(mach);
    }
    return 1;

} /* handle_data_attrs */

/*
 * delcare_data
 *
 * Common parsing routine for all non-LITERAL data declarations.
 */
static int
declare_data (expr_ctx_t ctx, scopectx_t scope, lextype_t lt, decltype_t dt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    name_t *owner = expr_current_routine(ctx);
    dataclass_t dc;
    int status = 1;
    symscope_t sc;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    switch (lt) {
        case LEXTYPE_DCL_STACKLOCAL:
            if (owner == 0) {
                expr_signal(ctx, STC__LCLNORTN);
            }
            dc = DCLASS_STACKONLY;
            break;
        case LEXTYPE_DCL_LOCAL:
            if (owner == 0) {
                expr_signal(ctx, STC__LCLNORTN);
            }
            dc = DCLASS_STKORREG;
            break;
        case LEXTYPE_DCL_REGISTER:
            if (owner == 0) {
                expr_signal(ctx, STC__REGNORTN);
            }
            dc = DCLASS_REGISTER;
            break;
        default:
            if (dt == DCL_NORMAL) {
                owner = scope_sclass_psectname(scope, SCLASS_OWN);
            } else if (dt == DCL_GLOBAL) {
                owner = scope_sclass_psectname(scope, SCLASS_GLOBAL);
            }
            dc = DCLASS_STATIC;
            break;
    }

    sc = (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL
          : (dt == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
             : SYMSCOPE_LOCAL));

    while (status != 0) {
        data_attr_t attr;

        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }

        memset(&attr, 0, sizeof(attr));
        attr.dclass = dc;
        attr.flags = SYM_M_PENDING | (dt == DCL_FORWARD ? SYM_M_FORWARD : 0);
        attr.owner = owner;
        attr.sc = sc;
        np = datasym_declare(scope, namestr, &attr, pos);
        if (np == 0) {
            expr_signal(ctx, STC__INTCMPERR, "declare_data[1]");
        }

        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, &attr, np);
            if (!status) {
                expr_signal(ctx, STC__SYNTAXERR);
            }
        } else {
            attr.units = machine_scalar_units(expr_machinedef(ctx));
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status >= 0) {
            attr.flags &= ~SYM_M_PENDING;
            if (!datasym_attr_update(np, &attr)) {
                expr_signal(ctx, STC__INTCMPERR, "declare_data[2]");
                status = -1;
            }
        }
        if (status < 0) {
            string_free(expr_strctx(ctx), namestr);
            name_undeclare(scope, np, 0);
            break;
        }
        string_free(expr_strctx(ctx), namestr);
    }

    return 1;

} /* declare_data */

/*
 * declare_bind
 *
 * Common parsing for BIND data declarations.
 */
static int
declare_bind (expr_ctx_t ctx, scopectx_t scope, decltype_t dt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np;
    int status = 1;
    expr_node_t *exp;
    static lextype_t delims[2] = { LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COMMA };

    while (status != 0) {
        data_attr_t attr;
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            expr_signal(ctx, STC__OPEREXP, "=");
            return 0;
        }
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
            return 0;
        }
        if (expr_libgen(ctx) && !expr_is_ctce(exp)) {
            expr_signal(ctx, STC__INVLIBDCL);
        }
        // XXX manual says allowed LTCEs are more restrictive
        //     than in other contexts?
        if (dt == DCL_GLOBAL && !expr_is_ltce(exp)) {
            expr_signal(ctx, STC__LTCEREQD);
        }
        memset(&attr, 0, sizeof(attr));
        attr.flags = SYM_M_PENDING | SYM_M_BIND;
        if (expr_is_ltce(exp)) {
            // Set these so that we know this name also represents
            // an LTCE.
            attr.owner = scope_sclass_psectname(scope,
                                                (dt == DCL_GLOBAL
                                                 ? SCLASS_GLOBAL
                                                 : SCLASS_PLIT));
            attr.dclass = DCLASS_STATIC;
        } else {
            attr.dclass = DCLASS_STKORREG;
        }
        attr.ivlist = expr_initval_add(ctx, 0, exp, machine_scalar_units(mach));
        attr.units = machine_scalar_units(mach);
        attr.sc = (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL : SYMSCOPE_LOCAL);
        np = datasym_declare(scope, namestr, &attr, pos);
        if (np == 0) {
            expr_signal(ctx, STC__INTCMPERR, "declare_bind");
        }

        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, dt, &attr, np);
            if (!status) {
                expr_signal(ctx, STC__SYNTAXERR);
            }
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status < 0) {
            string_free(expr_strctx(ctx), namestr);
            if (np != 0) name_undeclare(scope, np, 0);
            expr_signal(ctx, STC__DELIMEXP, ",");
            break;
        }
        attr.flags &= ~SYM_M_PENDING;
        datasym_attr_update(np, &attr);
        string_free(expr_strctx(ctx), namestr);
    }

    return 1;

} /* declare_bind */

/*
 * declare_map
 *
 * MAP declarations.  MAP simply applies a different set of attributes
 * to an already-declared name.
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
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }
        np = datasym_search(scope, namestr, &attr);
        if (np == 0) {
            expr_signal(ctx, STC__MAPNONDCL, namestr);
            return 0;
        }
        if (name_scope(np) != scope) {
            attr.flags |= SYM_M_PENDING;
            attr.sc = SYMSCOPE_LOCAL;
            np = datasym_declare(scope, namestr, &attr, pos);
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            status = handle_data_attrs(ctx, scope, DCL_MAP, &attr, np);
            if (!status) {
                expr_signal(ctx, STC__SYNTAXERR);
            } else {
                attr.flags &= ~SYM_M_PENDING;
                datasym_attr_update(np, &attr);
            }
        } else {
            expr_signal(ctx, STC__DELIMEXP, ":");
        }
        status = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, 0, 1);
        if (status < 0) {
            string_free(expr_strctx(ctx), namestr);
            expr_signal(ctx, STC__DELIMEXP, ",");
            break;
        }
        string_free(expr_strctx(ctx), namestr);
    }

    return 1;

} /* declare_map */

/*
 * parse_formals
 *
 * Parse a routine's formal parameter list.  Builds a scope
 * for the parameter names and namereflists to track the positions
 * of the input and output parameters.
 *
 * Returns: 1 on success
 * 0 on failure
 */
static int
parse_formals (expr_ctx_t ctx, scopectx_t curscope,
               scopectx_t *argtable, namereflist_t *inargs,
               namereflist_t *outargs)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
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
        attr.dclass = DCLASS_ARG;
        attr.units = machine_scalar_units(mach);
        attr.width = machine_scalar_bits(mach);
        attr.sc = SYMSCOPE_LOCAL;
        if (parse_decl_name(pctx, &namestr, &pos)) {
            if (*argtable == 0) {
                *argtable = scope_begin(expr_namectx(ctx), 0);
            }
            np = datasym_declare(*argtable, namestr, &attr, pos);
            string_free(expr_strctx(ctx), namestr);
            if (np == 0) {
                expr_signal(ctx, STC__INTCMPERR, "parse_formals[1]");
                break;
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                if (!handle_data_attrs(ctx, curscope, DCL_MAP, &attr, np)) {
                    expr_signal(ctx, STC__SYNTAXERR);
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

/*
 * handle_routine_attrs
 *
 * Parse all attributes for routines.
 */
static int
handle_routine_attrs (expr_ctx_t ctx, scopectx_t scope,
                      decltype_t dt, routine_attr_t *attr)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    int saw_psect, saw_novalue; // XXX need to handle linkage here
    int did1;
    int which;
    static lextype_t delims[3] = { LEXTYPE_DELIM_SEMI,
                                   LEXTYPE_DELIM_COMMA,
                                   LEXTYPE_OP_ASSIGN};

    saw_psect = saw_novalue = 0;

    if (attr->flags & SYM_M_BIND) {
        saw_psect = -1;
    }

    do {
        did1 = 0;
        if (!saw_psect) {
            saw_psect = attr_psect(ctx, &attr->owner);
            if (saw_psect) {
                did1 = 1;
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
        expr_signal(ctx, STC__RTNATTEXP);
        return -1;
    }

    return which;

} /* handle_routine_attrs */

/*
 * declare_routine
 *
 * Handles all ROUTINE declarations, including BIND ROUTINEs.
 */
static int
declare_routine (expr_ctx_t ctx, scopectx_t scope, decltype_t dt, int is_bind)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    gencodectx_t gctx = expr_gencodectx(ctx);
    strdesc_t *namestr;
    textpos_t pos;
    name_t *np, *psname;
    symscope_t sc;
    int which;
    static lextype_t delims[4] = { LEXTYPE_DELIM_SEMI,
        LEXTYPE_DELIM_COMMA, LEXTYPE_OP_ASSIGN, LEXTYPE_DELIM_COLON };



    if (is_bind) {
        psname = (dt == DCL_GLOBAL
                   ? scope_sclass_psectname(scope, SCLASS_GLOBAL)
                   : expr_current_routine(ctx));
    } else {
        psname = scope_sclass_psectname(scope, SCLASS_CODE);
    }
    sc = (dt == DCL_GLOBAL ? SYMSCOPE_GLOBAL
          : (dt == DCL_EXTERNAL ? SYMSCOPE_EXTERNAL
             : SYMSCOPE_LOCAL));

    while (1) {
        routine_attr_t attr;
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            return 0;
        }
        memset(&attr, 0, sizeof(attr));
        attr.flags = (is_bind ? SYM_M_BIND|SYM_M_PENDING : 0);
        attr.owner = psname;
        attr.sc = sc;

        if (dt != DCL_EXTERNAL && dt != DCL_FORWARD) {
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                if (!parse_formals(ctx, scope, &attr.argscope,
                                   &attr.inargs, &attr.outargs)) {
                    expr_signal(ctx, STC__SYNTAXERR);
                    return 0;
                }
            }
        }
        which = parser_expect_oneof(pctx, QL_NORMAL, delims, 4, 0, 1);
        if (which == 3) {  // the colon
            which = handle_routine_attrs(ctx, scope, dt, &attr);
            if (which < 0) {
                return 0;
            }
        }
        if (dt == DCL_FORWARD) attr.flags |= SYM_M_FORWARD;
        np = rtnsym_declare(scope, namestr, &attr, pos);
        if (is_bind || dt == DCL_NORMAL || dt == DCL_GLOBAL) {
            expr_node_t *exp;

            if (which != 2) { // i.e., the '='
                expr_signal(ctx, STC__OPEREXP, "=");
                return 0;
            }
            if (!is_bind) {
                expr_push_routine(ctx, np);
                if (gctx != 0) gencode_routine_begin(gctx, np);
                if (attr.argscope != 0) {
                    parser_scope_push(pctx, attr.argscope);
                }
            }
            if (!expr_parse_expr(ctx, &exp)) {
                expr_signal(ctx, STC__EXPREXP);
            }
            if ((is_bind || (attr.flags & SYM_M_NOVALUE) == 0)
                && !expr_has_value(exp)) {
                if (expr_type(exp) == EXPTYPE_PRIM_BLK) {
                    expr_node_t *valexp = expr_blk_valexp(exp);
                    // Check for final expression being non-semi-terminated
                    // RETURN with value, and convert it to being just the
                    // value.
                    if (valexp != 0 && expr_type(valexp) == EXPTYPE_CTRL_RET) {
                        expr_node_t *retval = expr_exit_value(valexp);
                        if (retval != 0) {
                            valexp = exprseq_remtail(expr_blk_seq(exp));
                            exprseq_instail(expr_blk_seq(exp), retval);
                            expr_blk_valexp_set(exp, retval);
                            expr_has_value_set(exp, 1);
                            if (valexp != 0) expr_exit_value_set(valexp, 0);
                            expr_node_free(ctx, valexp);
                        }
                    }
                }
            }
            if (is_bind) {
                symctx_t symctx = expr_symctx(ctx);
                machinedef_t *mach = expr_machinedef(ctx);
                if (expr_libgen(ctx) && !expr_is_ctce(exp)) {
                    expr_signal(ctx, STC__INVLIBDCL);
                }
                attr.ivlist = initval_expr_add(symctx, 0, 1, exp,
                                               machine_addr_bits(mach), 0);
                attr.flags &= ~SYM_M_PENDING;
                rtnsym_attr_update(np, &attr);
            } else {
                if (attr.argscope != 0) {
                    parser_scope_pop(pctx);
                }
                expr_pop_routine(ctx);
                rtnsym_expr_set(np, exp);
                if (gctx != 0) gencode_routine_end(gctx, np);
            }
        }

        if (which == 0) { // the semicolon
            break;
        } else if (which == 1) { // the comma
            continue;
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
            break;
        }
    }
    return 1;
}

/*
 * declare_require
 *
 * Handles REQUIRE "declarations".
 */
static int
declare_require (parse_ctx_t pctx)
{
    lexeme_t *lex;
    strdesc_t *str;
    char *fname;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__STRINGEXP);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ";");
    }
    str = lexeme_text(lex);
    if (parser_fopen(pctx, str->ptr, str->len, &fname)) {
        listing_require_begin(parser_lstgctx(pctx), fname, strlen(fname));
    } else {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__REQFILERR, str);
    }
    lexeme_free(parser_lexmemctx(pctx), lex);
    return 1;

} /* declare_require */

/*
 * declare_library
 *
 * Handles LIBRARY "declarations".
 */
static int
declare_library (parse_ctx_t pctx)
{
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__STRINGEXP);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ";");
    }
    parser_lib_process(pctx, lexeme_text(lex));
    return 1;

} /* declare_library */

/*
 * undeclare
 *
 * Handles UNDECLAREs.
 */
static int
undeclare (parse_ctx_t pctx, scopectx_t scope)
{
    lexeme_t *lex;

    while (1) {
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_NAME, &lex, 1)) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__NAMEEXP);
        } else {
            sym_undeclare(scope, lexeme_text(lex), parser_curpos(pctx));
        }
        lexeme_free(parser_lexmemctx(pctx), lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ";");
    }
    return 1;

} /* undeclare */

/*
 * BUILTIN
 *
 * Parses BUILTIN declarations, making the builtin name visible in
 * the current scope.
 */
static int
declare_builtin (parse_ctx_t pctx, scopectx_t scope)
{
    strdesc_t *namestr;
    textpos_t pos;

    while (1) {
        namestr = 0;
        if (parse_decl_name(pctx, &namestr, &pos)) {
            name_declare_builtin(scope, namestr, pos); // errors handled inside
            string_free(parser_strctx(pctx), namestr);
        } else {
            log_signal(parser_logctx(pctx), parser_curpos(pctx),
                       STC__NAMEEXP);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ";");
    }
    return 1;

}

/*
 * declarations_init
 *
 * Initialization routine.
 */
declctx_t
declarations_init (expr_ctx_t ctx, parse_ctx_t pctx,
                   scopectx_t kwdscope, machinedef_t *mach)
{
    unsigned int i;
    literal_attr_t attr;
    name_t *errson, *errsoff;
    declctx_t dctx;
    static strdesc_t bpdsc[4] = {
        STRDEF("%BPUNIT"), STRDEF("%BPADDR"), STRDEF("%BPVAL"),
        STRDEF("%UPVAL") };
    static strdesc_t errswitch = STRDEF("ERRS");

    for (i = 0; i < sizeof(decl_names)/sizeof(decl_names[0]); i++) {
        name_declare(kwdscope, &decl_names[i], 0, 0, 0, 0);
    }

    parser_lexfunc_register(pctx, ctx, LEXTYPE_LXF_ASSIGN, parse_ASSIGN);

    dctx = malloc(sizeof(struct declctx_s));
    memset(dctx, 0, sizeof(struct declctx_s));

    dctx->mctx = macros_init(kwdscope, ctx);
    machine_psects_init(mach, kwdscope);

    attr.width = machine_unit_bits(mach);
    attr.flags = SYM_M_RESERVED;
    attr.value = machine_unit_bits(mach);
    attr.sc = SYMSCOPE_LOCAL;
    litsym_declare(kwdscope, &bpdsc[0], &attr, 0);
    attr.value = machine_addr_bits(mach);
    litsym_declare(kwdscope, &bpdsc[1], &attr, 0);
    attr.value = machine_scalar_bits(mach);
    litsym_declare(kwdscope, &bpdsc[2], &attr, 0);
    attr.value = machine_scalar_units(mach);
    litsym_declare(kwdscope, &bpdsc[3], &attr, 0);
    structures_init(ctx, kwdscope);
    switch_toggle_declare(kwdscope, &errswitch, toggle_errs, 0, 0, &errson, &errsoff);

    return dctx;

} /* declarations_init */

/*
 * declarations_finish
 *
 * Module cleanup.
 */
void
declarations_finish (declctx_t dctx)
{
    if (dctx == 0) return;
    macros_finish(dctx->mctx);
    free(dctx);
    
} /* declarations_finish */

/*
 * parse_declaration
 *
 * Common routine for parsing declarations that are allowed inside a
 * block (i.e., not MODULEs).
 */
int
parse_declaration (expr_ctx_t ctx)
{
    lextype_t lt;
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    scopectx_t scope = parser_scope_get(pctx);
    int libgen = expr_libgen(ctx);
    int status;
    int which;
    static lextype_t pfx[] = { LEXTYPE_DCL_GLOBAL, LEXTYPE_DCL_EXTERNAL,
                               LEXTYPE_DCL_FORWARD };
    static decltype_t  dtypes[] = { DCL_NORMAL, DCL_GLOBAL, DCL_EXTERNAL, DCL_FORWARD };
    static lextype_t allowed[3][4] = {
        { LEXTYPE_DCL_LITERAL, LEXTYPE_DCL_ROUTINE, LEXTYPE_DCL_REGISTER,
          LEXTYPE_DCL_BIND },
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
        case LEXTYPE_DCL_BUILTIN:
            status = declare_builtin(pctx, scope);
            break;
        case LEXTYPE_DCL_BIND:
            if (libgen && dtypes[which] == DCL_GLOBAL) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_ROUTINE, 0, 1)) {
                status = declare_routine(ctx, scope, dtypes[which], 1);
            } else {
                status = declare_bind(ctx, scope, dtypes[which]);
            }
            break;
        case LEXTYPE_DCL_GLOBAL:
        case LEXTYPE_DCL_OWN:
        case LEXTYPE_DCL_FORWARD:
        case LEXTYPE_DCL_REGISTER:
            if (libgen) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            // FALLTHROUGH
        case LEXTYPE_DCL_EXTERNAL:
            status = declare_data(ctx, scope, lt, dtypes[which]);
            break;
        case LEXTYPE_DCL_STACKLOCAL:
        case LEXTYPE_DCL_LOCAL:
            if (libgen) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_data(ctx, scope, lt, DCL_NORMAL);
            break;
        case LEXTYPE_DCL_MACRO:
        case LEXTYPE_DCL_KEYWORDMACRO:
            status = declare_macro(ctx, scope, lt);
            break;
        case LEXTYPE_DCL_COMPILETIME:
            status = declare_compiletime(ctx, scope, lt);
            break;
        case LEXTYPE_DCL_LITERAL:
            if (libgen && dtypes[which] == DCL_GLOBAL) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_literal(ctx, scope, dtypes[which]);
            break;
        case LEXTYPE_DCL_LABEL:
            if (libgen) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_label(pctx, scope);
            break;
        case LEXTYPE_DCL_PSECT:
            if (libgen) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_psect(ctx, scope);
            break;
        case LEXTYPE_DCL_STRUCTURE:
            status = declare_structure(ctx, scope);
            break;
        case LEXTYPE_DCL_MAP:
            if (libgen) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_map(ctx, scope);
            break;
        case LEXTYPE_DCL_FIELD:
            status = declare_field(ctx, scope);
            break;
        case LEXTYPE_DCL_REQUIRE:
            status = declare_require(pctx);
            break;
        case LEXTYPE_DCL_LIBRARY:
            status = declare_library(pctx);
            break;
        case LEXTYPE_DCL_UNDECLARE:
            status = undeclare(pctx, scope);
            break;
        case LEXTYPE_DCL_ROUTINE:
            if (libgen && dtypes[which] != DCL_EXTERNAL) {
                expr_signal(ctx, STC__INVLIBDCL);
            }
            status = declare_routine(ctx, scope, dtypes[which], 0);
            break;
        case LEXTYPE_DCL_SWITCHES:
            status = parse_switches(pctx, lt, LEXTYPE_DELIM_SEMI);            
            break;
        default:
            expr_signal(ctx, STC__SYNTAXERR);
            status = 0;
            break;
    }

    return status;

} /* parse_declaration */

/*
 * parse_ident
 *
 * Parses the IDENT module switch.
 */
static int
parse_ident (parse_ctx_t pctx, void *vctx, lextype_t dcltype, lexeme_t *lex)
{
    name_t *modnp = vctx;
    lexeme_t *idlex;
    strdesc_t *id;

    if (dcltype != LEXTYPE_DCL_MODULE) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__UNEXPSWIT,
                   lexeme_text(lex));
        return 0;
    }
    if (modsym_ident(modnp) != 0) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__SWITCHDUP,
                   lexeme_text(lex));
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, "=");
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &idlex, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__STRINGEXP);
        return 0;
    }
    id = string_copy(parser_strctx(pctx), 0, lexeme_text(idlex));
    modsym_ident_set(modnp, id);
    listing_ident_set(parser_lstgctx(pctx), id); // this copies the string
    lexeme_free(parser_lexmemctx(pctx), idlex);
    return 1;

} /* parse_ident */

/*
 * parse_main
 *
 * Parses the MAIN module switch.
 */
static int
parse_main (parse_ctx_t pctx, void *vctx, lextype_t dcltype, lexeme_t *lex)
{
    name_t *modnp = vctx;
    lexeme_t *idlex;
    strdesc_t *id;

    if (dcltype != LEXTYPE_DCL_MODULE) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__UNEXPSWIT,
                   lexeme_text(lex));
        return 0;
    }
    if (modsym_main(modnp) != 0) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__SWITCHDUP,
                   lexeme_text(lex));
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, "=");
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME, &idlex, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__NAMEEXP);
        return 0;
    }
    id = string_copy(parser_strctx(pctx), 0, lexeme_text(idlex));
    modsym_main_set(modnp, id);
    lexeme_free(parser_lexmemctx(pctx), idlex);
    return 1;

} /* parse_main */

/*
 * declare_module
 *
 * Parses a MODULE declaration.
 */
int
declare_module (expr_ctx_t ctx)
{
    gencodectx_t gctx = expr_gencodectx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    scopectx_t scope = parser_scope_get(pctx);
    strdesc_t *text;
    expr_node_t *blkexp;
    textpos_t pos;
    name_t *np;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_MODULE, 0, 0)) {
        expr_signal(ctx, STC__MODDCLEXP);
        return 0;
    }
    if (!parse_decl_name(pctx, &text, &pos)) {
        expr_signal(ctx, STC__NAMEEXP);
        return 0;
    }
    listing_mainscope_set(expr_lstgctx(ctx), scope);
    listing_name_set(expr_lstgctx(ctx), text);
    np = modsym_declare(scope, text, pos);
    if (np == 0) {
        expr_signal(ctx, STC__INTCMPERR, "declare_module");
        return 0;
    }
    switch_special_declare(parser_kwdscope(pctx), LEXTYPE_SWITCH_IDENT, parse_ident, np);
    switch_special_declare(parser_kwdscope(pctx), LEXTYPE_SWITCH_MAIN, parse_main, np);
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        parse_switches(pctx, LEXTYPE_DCL_MODULE, LEXTYPE_DELIM_RPAR);
    }

    if (gctx != 0) gencode_module_begin(gctx, np);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 0)) {
        expr_signal(ctx, STC__OPEREXP, "=");
        return 0;
    }
    if (!expr_parse_block(ctx, &blkexp)) {
        expr_signal(ctx, STC__SYNTAXERR);
        return 0;
    }
    modsym_block_set(np, blkexp);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DCL_ELUDOM, 0, 0)) {
        expr_signal(ctx, STC__KWDEXP, "ELUDOM");
    }

    if (gctx != 0) gencode_module_end(gctx, np);

    string_free(expr_strctx(ctx), text);
    text = modsym_main(np);
    if (text != 0) {
        scopectx_t blkscope = expr_blk_scope(blkexp);
        name_t *mainrtn = 0;
        if (blkscope != 0) {
            mainrtn = rtnsym_search(blkscope, text);
            if (mainrtn != 0) {
                if (rtnsym_expr(mainrtn) == 0) {
                    expr_signal(ctx, STC__MNTYPERR, text);
                }
            }
        }
        if (mainrtn == 0) {
            expr_signal(ctx, STC__NOMAIN, text);
        }
        string_free(expr_strctx(ctx), text);
    }

    return 1;

} /* declare_module */

/*
 * %ASSIGN(#name, n)
 *
 * Assign a value to a compiletime constant.
 */
static int
parse_ASSIGN (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    name_t *np;
    lexeme_t *lex;
    strdesc_t *lexstr;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }
    parser_next(pctx, QL_NAME, &lex);
    if (lex == 0 || lexeme_boundtype(lex) != LEXTYPE_NAME) {
        lexeme_free(expr_lexmemctx(ctx), lex);
        expr_signal(ctx, STC__CTNAMEXP);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    lexstr = lexeme_text(lex);
    np = name_search_typed(parser_scope_get(pctx), lexstr->ptr, lexstr->len,
                           LEXTYPE_NAME_COMPILETIME, 0);
    lexeme_free(expr_lexmemctx(ctx), lex);
    if (np == 0) {
        expr_signal(ctx, STC__CTNAMEXP);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ",");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!expr_parse_ctce(ctx, 0, &val)) {
        expr_signal(ctx, STC__EXPCTCE);
    } else {
        compiletime_assign(np, val);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 0)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    return 1;

} /* parse_ASSIGN */

/*
 * parse_libgen_declarations
 *
 * Parses a sequence of declarations for library generation.
 */
int
parse_libgen_declarations (expr_ctx_t ctx)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    scopectx_t scope = parser_scope_get(pctx);

    listing_mainscope_set(expr_lstgctx(ctx), scope);

    while (!parser_atend(pctx)) {
        parse_declaration(ctx);
    }

    return 1;

} /* parse_libgen_declarations */