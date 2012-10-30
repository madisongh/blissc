//
//  parser.c
//  blissc
//
//  Created by Matthew Madison on 10/28/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "parser.h"
#include "lexeme.h"
#include "lexer.h"
#include "nametable.h"

struct parse_ctx_s {
    scopectx_t      curscope;
    lexer_ctx_t     lexctx;
    int             lib_compile;
    enum { QL_NORMAL, QL_NAME, QL_MACRO } quotelevel;
    int             no_eof;
    int             do_quote, do_unquote;
};

static int is_valid_lib_decl(lexeme_t *lex);
static int is_module_decl(lexeme_t *lex);
static int macro_expand(parse_ctx_t pctx, name_t *macroname);

static int parse_QUOTE(parse_ctx_t);
static int parse_UNQUOTE(parse_ctx_t);
static int parse_EXPAND(parse_ctx_t);

#undef DODEF
#define DODEFS \
    DODEF(ASCII) \
    DODEF(ASCIC) \
    DODEF(ASCIZ) \
    DODEF(B) \
    DODEF(O) \
    DODEF(DECIMAL) \
    DODEF(X) \
    DODEF(C)

#define DODEF(name_) static int parse_##name_ (parse_ctx_t);
DODEFS
#undef DODEF
#define DODEF(name_) LEXDEF("%" #name_, parse_##name_, 0),

name_t parser_names[] = {
    DODEFS
    LEXDEF("%QUOTE", parse_QUOTE, NAME_M_QFUNC),
    LEXDEF("%UNQUOTE", parse_UNQUOTE, NAME_M_QFUNC),
    LEXDEF("%EXPAND", parse_EXPAND, NAME_M_QFUNC)
};

void
parser_init (scopectx_t kwdscope)
{
    int i;
    for (i = 0; i < sizeof(parser_names)/sizeof(parser_names[0]); i++) {
        name_insert(kwdscope, &parser_names[i]);
    }
}


int
parser_run (scopectx_t initscope,
            const char *fname, size_t fnlen, int is_lib)
{
    struct parse_ctx_s topctx;
    lexeme_t *lex;
    int ret = 0;

    memset(&topctx, 0, sizeof(topctx));
    topctx.curscope = initscope;
    topctx.lexctx = lexer_init(fname, fnlen);
    if (topctx.lexctx == 0) {
        /* XXX error condition */
        return 0;
    }
    topctx.lib_compile = is_lib;
    topctx.quotelevel = QL_NORMAL;
    topctx.no_eof = 0; // set when in macro def or lexical conditional

    if (is_lib) {
        int keepgoing = 1;
        while (keepgoing) {
            lex = parser_next(&topctx);
            if (lex == 0) {
                /* XXX error condition */
                return 0;
            }
            if (lex->type == LEXTYPE_END) {
                keepgoing = 0;
                ret = 1; // success
            } else if (is_valid_lib_decl(lex)) {
                parser_dispatch_t doit = lex->data.ptr;
                keepgoing = doit(&topctx);
            } else {
                /* XXX error condition */
                keepgoing = 0;
            }
        }
    } else {
        lex = parser_next(&topctx);
        if (is_module_decl(lex)) {
            parser_dispatch_t doit = lex->data.ptr;
            if (doit(&topctx)) {
                lex = parser_next(&topctx);
                if (lex->type != LEXTYPE_END) {
                    /* XXX error condition */
                } else {
                    ret = 1;
                }
            } else {
                /* XXX error condition */
            }
        } else {
            /* XXX error condition */
        }
    } /* lib or module? */

    lexer_finish(topctx.lexctx);

    return ret;
}

lexeme_t *
parser_next (parse_ctx_t pctx)
{
    lexeme_t *lex;
    name_t *np;
    int keepgoing = 1;
    int do_bind, do_expand;
    parser_dispatch_t doit;

    while (keepgoing) {
        lex = lexer_next(pctx->lexctx, pctx->curscope, pctx->no_eof);
        keepgoing = 0;
        np = lex->data.ptr;
        if (lex->type != LEXTYPE_IDENT || pctx->do_quote || np == 0
            || (np->nameflags & NAME_M_NOTDCL)) {
            pctx->do_quote = pctx->do_unquote = 0;
            break;
        }

        do_bind = do_expand = 0;
        switch (pctx->quotelevel) {
            case QL_MACRO:
                do_bind = pctx->do_unquote ||
                            (np->nametype == NAMETYPE_LEXNAME &&
                             np->name_lntype == LNTYPE_MACPARAM);
                do_expand = pctx->do_unquote == 2 ||
                            (np->nametype == NAMETYPE_LEXFUNC &&
                              (np->nameflags & NAME_M_QFUNC) != 0);
                break;
            case QL_NAME:
                do_bind = pctx->do_unquote ||
                            (np->nametype == NAMETYPE_LEXNAME &&
                             np->name_lntype == LNTYPE_MACRO);
                do_expand = pctx->do_unquote == 2 ||
                            np->nametype == NAMETYPE_LEXFUNC ||
                            (np->nametype == NAMETYPE_LEXNAME &&
                             np->name_lntype == LNTYPE_MACRO);
                break;
            case QL_NORMAL:
                do_bind = do_expand = 1;
                break;
        }

        pctx->do_quote = pctx->do_unquote = 0;
        if (do_bind) {
            if (np->nametype == NAMETYPE_KEYWORD ||
                (do_expand && np->nametype == NAMETYPE_LEXFUNC)) {
                doit = np->namedata.ptr;
                keepgoing = doit(pctx);
                lexeme_free(lex);
                lex = 0;
            } else if (np->nametype == NAMETYPE_LEXNAME &&
                       np->name_lntype != LNTYPE_MACRO) {
                lexer_insert(pctx->lexctx, lexeme_copy(np->namedata.ptr));
                keepgoing = 1;
                lexeme_free(lex);
                lex = 0;
            } else if (do_expand && np->nametype == NAMETYPE_LEXNAME &&
                       np->name_lntype == LNTYPE_MACRO) {
                keepgoing = macro_expand(pctx, np);
                lexeme_free(lex);
                lex = 0;
            } else {
                break;
            }
        } else {
            break;
        }

    } /* while keepgoing */

    return lex;
}

static int
parse_QUOTE (parse_ctx_t pctx)
{
    lexeme_t *lex;

    if (pctx->quotelevel < QL_NAME) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    if (lex == 0) {
        /* XXX error condition */
        return 1;
    }
    // %QUOTE only applies to names, lexical function names,
    // commas, and percent signs.
    if (lex->type != LEXTYPE_IDENT &&
        lex->type != LEXTYPE_DELIM_COMMA &&
        lex->type != LEXTYPE_DELIM_PERCENT) {
        /* XXX error condition */
    } else if (lex->type == LEXTYPE_IDENT) {
        name_t *np;
        np = lex->data.ptr;
        if (np == 0) {
            np = name_search(pctx->curscope, lex->data.val_string.ptr,
                             lex->data.val_string.len, 0);
        }
        if (np != 0 && np->nametype == NAMETYPE_KEYWORD) {
            /* XXX error condition */
        }
        pctx->do_quote = 1;
    } else {
        pctx->do_quote = 1;
    }
    // no matter what, put back the lexeme we just peeked at
    lexer_insert(pctx->lexctx, lex);
    return 1;
}

static int
parse_unquote_expand (parse_ctx_t pctx, int is_expand)
{
    lexeme_t *lex;

    if (pctx->quotelevel < (is_expand ? QL_MACRO : QL_NAME)) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    if (lex == 0) {
        /* XXX error condition */
        return 1;
    }
    if (lex->type != LEXTYPE_IDENT) {
        /* XXX error condition */
    } else {
        name_t *np = lex->data.ptr;
        if (np == 0) {
            np = name_search(pctx->curscope, lex->data.val_string.ptr,
                             lex->data.val_string.len, 0);
        }
        if (np == 0) {
            /* XXX error condition */
        } else if (np->nametype == NAMETYPE_KEYWORD ||
            (is_expand && !((np->nametype == NAMETYPE_LEXFUNC) ||
                            (np->nametype == NAMETYPE_LEXNAME &&
                             np->name_lntype == LNTYPE_MACRO)))) {
            /* XXX error condition */
        } else {
            pctx->do_unquote = 1 + is_expand;
        }
    }
    lex->next = 0;
    lexer_insert(pctx->lexctx, lex);
    return 1;
}

static int
parse_UNQUOTE (parse_ctx_t pctx)
{
    return parse_unquote_expand(pctx, 0);
}

static int
parse_EXPAND (parse_ctx_t pctx)
{
    return parse_unquote_expand(pctx, 1);
}

static int
parse_string_literal (parse_ctx_t pctx, int whichtype)
{
    lexeme_t *lex = parser_next(pctx);
    if (lex->type != LEXTYPE_STRING || (whichtype == 2 && lex->data.val_string.len > 255)) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        lex->next = 0;
        if (whichtype != 0) {
            char *newstr;
            newstr = malloc(lex->data.val_string.len+1);
            if (newstr == 0) {
                    /* XXX error condition */
            } else {
                memcpy(newstr+(whichtype-1), lex->data.val_string.ptr, lex->data.val_string.len);
                if (whichtype == 1) {
                    newstr[lex->data.val_string.len+1] = '\0';
                } else {
                    *newstr = lex->data.val_string.len;
                    lex->type = LEXTYPE_CSTRING;
                }
                free(lex->data.val_string.ptr);
                lex->data.val_string.ptr = newstr;
                lex->data.val_string.len += 1;
            }
        }
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;
}

static int parse_ASCII (parse_ctx_t pctx) { return parse_string_literal(pctx, 0); }
static int parse_ASCIZ (parse_ctx_t pctx) { return parse_string_literal(pctx, 1); }
static int parse_ASCIC (parse_ctx_t pctx) { return parse_string_literal(pctx, 2); }

static int
parse_numeric_literal (parse_ctx_t pctx, int base)
{
    lexeme_t *lex = parser_next(pctx);
    long numval;

    if (lex->type != LEXTYPE_STRING) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        char *cp;
        lex->next = 0;
        lex->type = LEXTYPE_NUMERIC;
        errno = 0;
        // XXX probably need to do this ourselves, rather than rely on C lib
        numval = strtol(lex->data.val_string.ptr, &cp, base);
        if (errno != 0 || (cp-lex->data.val_string.ptr) != lex->data.val_string.len) {
            /* XXX error condition */
            lexeme_free(lex);
        }
        // XXX need to validate that the value fits into the
        // target machine's word length
        free(lex->data.val_string.ptr);
        lex->data.val_signed = numval;
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;
}

static int parse_B (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 2); }
static int parse_O (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 8); }
static int parse_X (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 16); }
static int parse_DECIMAL (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 10); }

static int
parse_C (parse_ctx_t pctx) {
    lexeme_t *lex = parser_next(pctx);
    if (lex->type != LEXTYPE_STRING || lex->data.val_string.len != 1) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        char ch = *lex->data.val_string.ptr & 0x7f;
        free(lex->data.val_string.ptr);
        lex->type = LEXTYPE_NUMERIC;
        lex->data.val_signed = ch;
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;
}