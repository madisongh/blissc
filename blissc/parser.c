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
#include "strings.h"
#include "expression.h"

struct parse_ctx_s {
    scopectx_t      kwdscope, curscope;
    void            *cctx;
    lexer_ctx_t     lexctx;
    int             lib_compile;
    enum { QL_NORMAL, QL_NAME, QL_MACRO } quotelevel;
    int             no_eof;
    int             do_quote, do_unquote;
};

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
    DODEF(C) \
    DODEF(STRING) \
    DODEF(EXACTSTRING) \
    DODEF(CHARCOUNT) \
    DODEF(CHAR) \
    DODEF(EXPLODE) \
    DODEF(REMOVE) \
    DODEF(NAME) \
    DODEF(QUOTENAME) \
    DODEF(NULL) \
    DODEF(IDENTICAL) \
    DODEF(ISSTRING)
/*
 DODEF(CTCE) \
 DODEF(DECLARED) \
 DODEF(INFORM) \
 DODEF(MESSAGE) \
 DODEF(NUMBER) \
 DODEF(PRINT) \
 DODEF(REQUIRE) \
 DODEF(SBTTL) \
 DODEF(TITLE) \
 DODEF(WARN)
*/
#define DODEF(name_) static int parse_##name_ (parse_ctx_t);
DODEFS
#undef DODEF
#define DODEF(name_) LEXDEF("%" #name_, parse_##name_, 0),

static name_t parser_names[] = {
    DODEFS
    LEXDEF("%QUOTE", parse_QUOTE, NAME_M_QFUNC),
    LEXDEF("%UNQUOTE", parse_UNQUOTE, NAME_M_QFUNC),
    LEXDEF("%EXPAND", parse_EXPAND, NAME_M_QFUNC)
};
#undef DODEFS
#undef DODEF

static lexeme_t errlex = { 0, LEXTYPE_NONE };

static lextype_t opener[3] = {
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_LBRACK };
static lextype_t closer[3] = {
    LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RANGLE, LEXTYPE_DELIM_RBRACK };

/*
 *  --- Public API for this module ---
 */

/*
 * parser_init
 *
 * Initializes the parser by registering the lexical functions
 * in a name table (whose scope is passed by the caller) and
 * allocating a context block.  The caller may pass in a context
 * pointer to be stored in the block.
 */
parse_ctx_t
parser_init (scopectx_t kwdscope, void *cctx)
{
    parse_ctx_t pctx;
    int i;

    for (i = 0; i < sizeof(parser_names)/sizeof(parser_names[0]); i++) {
        name_insert(kwdscope, &parser_names[i]);
    }
    pctx = malloc(sizeof(struct parse_ctx_s));
    if (pctx != 0) {
        memset(pctx, 0, sizeof(struct parse_ctx_s));
        pctx->kwdscope = (kwdscope == 0 ? scope_begin(0) : kwdscope);
        pctx->curscope = pctx->kwdscope;
        lexer_init(pctx->kwdscope);
        pctx->cctx = cctx;
        pctx->quotelevel = QL_NORMAL;
    }

    return pctx;
} /* parser_init */

/*
 * parser_finish
 *
 * Cleans up a parsing context.
 */
void
parser_finish (parse_ctx_t pctx)
{
    if (pctx->lexctx != 0) {
        lexer_finish(pctx->lexctx);
    }
    scope_end(pctx->curscope);
    free(pctx);
} /* parser_finish */

/*
 * parser_fopen
 *
 * Begin parsing a file.
 */
int
parser_fopen (parse_ctx_t pctx, const char *fname, size_t fnlen)
{
    pctx->curscope = scope_begin(pctx->curscope);
    if (pctx->curscope == 0) {
        return 0;
    }
    pctx->lexctx = lexer_fopen(fname, fnlen);
    if (pctx->lexctx == 0) {
        return 0;
    }
    return 1;
} /* parser_fopen */

/*
 * parser_get_cctx
 *
 * Retrieves the 'cctx' pointer from the parsing
 * context block.
 */
void *
parser_get_cctx (parse_ctx_t pctx) {
    return pctx->cctx;
} /* parser_get_cctx */

/*
 * parser_insert
 *
 * Insert a lexeme to the front of the parsing stream.
 */
void
parser_insert (parse_ctx_t pctx, lexeme_t *lex)
{
    lexer_insert(pctx->lexctx, lex);
} /* parser_insert */

/*
 * parser_next
 *
 * Retrieve the next lexeme from the parse stream.  This is
 * the main workhorse routine for the parser.  It handles
 * quoting levels and binding of compile-time names (literals,
 * COMPILETIME variables) to their values, expanding macros,
 * and dispatching to process keywords and lexical functions.
 */
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
        if (lex->type == LEXTYPE_NONE || lex->type == LEXTYPE_END) {
            break;
        }
        np = lex->data.ptr;
        if (lex->type != LEXTYPE_IDENT || pctx->do_quote) {
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
        if (do_bind || do_expand) {
            if ((do_bind && np->nametype == NAMETYPE_KEYWORD) ||
                (do_expand && np->nametype == NAMETYPE_LEXFUNC)) {
                doit = np->namedata.ptr;
                keepgoing = doit(pctx);
                lexeme_free(lex);
                lex = 0;
            } else if (do_bind && np->nametype == NAMETYPE_LEXNAME &&
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
} /* parser_next */

/*
 * parser_skip_to_delim
 *
 * Utility routine to consume lexemes until hitting
 * the specified delimiter (or end of stream).
 */
void
parser_skip_to_delim (parse_ctx_t pctx, lextype_t delimtype)
{
    lexeme_t *lex;

    for (lex = parser_next(pctx); lex->type != LEXTYPE_END &&
         lex->type != LEXTYPE_NONE && lex->type != delimtype;
         lex = parser_next(pctx)) {
        lexeme_free(lex);
    }
    
}

/*
 * --- end of public API ---
 *
 * All of the following routines are internal only.
 *
 */
static int
macro_expand (parse_ctx_t pctx, name_t *macroname)
{
    /* XXX - placeholder */
    return 1;
}

/*
 * %QUOTE
 *
 * Prevents the next lexeme from being bound (i.e., replaced
 * with its value, if it's a compile-time name, like a macro
 * or literal).  Only permitted at name-quote or macro-quote level.
 */
static int
parse_QUOTE (parse_ctx_t pctx)
{
    lexeme_t *lex;

    if (pctx->quotelevel < QL_NAME) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    // %QUOTE only applies to names, lexical function names,
    // commas, and percent signs.
    if (lex->type != LEXTYPE_IDENT &&
        lex->type != LEXTYPE_DELIM_COMMA &&
        lex->type != LEXTYPE_DELIM_PERCENT) {
        /* XXX error condition */
    } else if (lex->type == LEXTYPE_IDENT) {
        name_t *np = lex->data.ptr;
        if (np->nametype == NAMETYPE_KEYWORD) {
            /* XXX error condition */
        }
        pctx->do_quote = 1;
    } else {
        pctx->do_quote = 1;
    }
    // no matter what, put back the lexeme we just peeked at
    lexer_insert(pctx->lexctx, lex);
    return 1;
} /* parse_QUOTE */

/*
 * parse_unquote_expand
 *
 * Common handler for %UNQUOTE and %EXPAND.
 * %UNQUOTE - binds the next lexeme, if it's a name.
 * %EXPAND  - binds the next lexeme, and expands it if it's a
 *            lexical function or a macro.
 */
static int
parse_unquote_expand (parse_ctx_t pctx, int is_expand)
{
    lexeme_t *lex;

    if (pctx->quotelevel < (is_expand ? QL_MACRO : QL_NAME)) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    if (lex->type != LEXTYPE_IDENT) {
        /* XXX error condition */
    } else {
        name_t *np = lex->data.ptr;
        if (np->nametype == NAMETYPE_KEYWORD ||
            np->nametype == NAMETYPE_UNDECLARED ||
            (is_expand && !((np->nametype == NAMETYPE_LEXFUNC) ||
                            (np->nametype == NAMETYPE_LEXNAME &&
                             np->name_lntype == LNTYPE_MACRO)))) {
            /* XXX error condition */
        } else {
            pctx->do_unquote = 1 + is_expand;
        }
    }
    lexer_insert(pctx->lexctx, lex);
    return 1;
} /* parse_unquote_expand */

/*
 * %UNQUOTE
 *
 * Binds (i.e., replaces the name with its value, if it's
 * a compile-time variable or a literal) the next lexeme.
 * Only permitted at name-quote or macro-quote level.
 */
static int
parse_UNQUOTE (parse_ctx_t pctx)
{
    return parse_unquote_expand(pctx, 0);
} /* parse_UNQUOTE */

/*
 * %EXPAND
 *
 * Expands the next lexeme, which must be either a
 * lexical function call or a macro name.  Only permitted
 * at macro-quote level.
 */
static int
parse_EXPAND (parse_ctx_t pctx)
{
    return parse_unquote_expand(pctx, 1);
} /* parse_EXPAND */

/*
 * parse_string_literal
 *
 * Forms a string literal lexeme from a string expression.
 * If 'whichtype' is 0, normal string.
 * If 'whichtype' is 1, null-terminated (%ASCIZ).
 * If 'whichtype' is 2, counted (%ASCIC).
 */
static int
parse_string_literal (parse_ctx_t pctx, int whichtype)
{
    lexeme_t *lex = parser_next(pctx);
    if (lex->type != LEXTYPE_STRING || (whichtype == 2 && lex->data.val_string.len > 255)) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        if (whichtype == 1) {
            static strdesc_t nullchr = STRZDEF("");
            string_append(&lex->data.val_string, &nullchr);
        } else if (whichtype == 2) {
            strdesc_t *acstr = ascic_string_from_chrs(0, lex->data.val_string.ptr,
                                                      lex->data.val_string.len);
            if (acstr != 0) {
                string_free(&lex->data.val_string);
                memcpy(&lex->data.val_string, acstr, sizeof(strdesc_t));
                lex->type = LEXTYPE_CSTRING;
            }
        }
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;
} /* parse_string_literal */

/*
 * %ASCII's', %ASCIZ's', %ASCIC's'
 *
 * String literals (plain, null-terminated, counted).
 *
 * Note that 's' can be a string expression (e.g., %STRING(...) or
 * another instance of %ASCI[IZ]).
 */
static int parse_ASCII (parse_ctx_t pctx) { return parse_string_literal(pctx, 0); }
static int parse_ASCIZ (parse_ctx_t pctx) { return parse_string_literal(pctx, 1); }
static int parse_ASCIC (parse_ctx_t pctx) { return parse_string_literal(pctx, 2); }

/*
 * parse_numeric_literal
 *
 * Common routine for parsing strings as numeric literals.
 *
 * Can begin with +/- sign, followed by one or more digits
 * in the specified 'base'.
 */
static int
parse_numeric_literal (parse_ctx_t pctx, int base)
{
    lexeme_t *lex = parser_next(pctx);
    long numval;

    if (lex->type != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
    } else {
        char *cp, buf[32];
        lex->type = LEXTYPE_NUMERIC;
        errno = 0;
        // XXX probably need to do this ourselves, rather than rely on C lib
        if (lex->data.val_string.len >= sizeof(buf)) {
            cp = buf + sizeof(buf)-1;
        } else {
            cp = buf+lex->data.val_string.len;
        }
        memcpy(buf, lex->data.val_string.ptr, cp-buf);
        *cp = '\0';
        numval = strtol(buf, &cp, base);
        if (errno != 0 || (cp-buf) != lex->data.val_string.len) {
            /* XXX error condition */
            lexeme_free(lex);
        } else {
            // XXX need to validate that the value fits into the
            // target machine's word length
            string_free(&lex->data.val_string);
            lex->data.val_signed = numval;
            lexer_insert(pctx->lexctx, lex);
        }
    }
    return 1;
} /* parse_numeric_literal */

/*
 * %B's', %O's', %X's', %DECIMAL's'
 *
 * Binary, octal, hexadecimal, and decimal literals.
 */
static int parse_B (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 2); }
static int parse_O (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 8); }
static int parse_X (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 16); }
static int parse_DECIMAL (parse_ctx_t pctx) { return parse_numeric_literal(pctx, 10); }

/*
 * %C'c'
 *
 * Numeric (ASCII) value of the character 'c'.
 *
 */
static int
parse_C (parse_ctx_t pctx) {
    lexeme_t *lex = parser_next(pctx);
    if (lex->type != LEXTYPE_STRING || lex->data.val_string.len != 1) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        char ch = *lex->data.val_string.ptr & 0x7f;
        string_free(&lex->data.val_string);
        lex->type = LEXTYPE_NUMERIC;
        lex->data.val_signed = ch;
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;
} /* parse_C */

/*
 * string_params
 *
 * Parses parameters for a %STRING-style lexical
 * function.  If 'already_have_open_paren' is non-zero,
 * the calling function has already parsed the opening
 * parenthesis for the parameter list (e.g., %EXACTSTRING).
 *
 */
static lexeme_t *
string_params (parse_ctx_t pctx, int already_have_open_paren)
{
    lexeme_t *lex;
    char      stringresult[1024];
    size_t    resultlen = 0;
    int       ql_was_normal = 0;

    if (!already_have_open_paren) {
        lex = parser_next(pctx);
        if (lex->type != LEXTYPE_DELIM_LPAR) {
            // put back the lexeme before returning? right?
            lexer_insert(pctx->lexctx, lex);
            /* XXX error condition */
            return &errlex;
        }
        lexeme_free(lex);
    }

    if (pctx->quotelevel == QL_NORMAL) {
        pctx->quotelevel = QL_NAME;
        ql_was_normal = 1;
    } else {
        /* XXX would this ever happen? */
    }

    while (1) {
        lex = parser_next(pctx);
        if (lex->type == LEXTYPE_DELIM_RPAR) {
            lexeme_free(lex);
            break;
        }
        // It's OK to have a null argument
        if (lex->type == LEXTYPE_DELIM_COMMA) {
            lexeme_free(lex);
            continue;
        }
        if (lex->type == LEXTYPE_NUMERIC) {
            size_t len;
            len = snprintf(stringresult+resultlen,
                           sizeof(stringresult)-resultlen,
                           "%ld", lex->data.val_signed);
            resultlen += len;
            lexeme_free(lex);
        } else if (lex->type == LEXTYPE_STRING ||
                   lex->type == LEXTYPE_CSTRING) {
            char *strp = lex->data.val_string.ptr;
            size_t len = lex->data.val_string.len;
            if (lex->type == LEXTYPE_CSTRING) {
                len -= 1;
                strp += 1;
            }
            if (len > sizeof(stringresult)-resultlen) {
                len = sizeof(stringresult)-resultlen;
            }
            memcpy(stringresult+resultlen, strp, len);
            resultlen += len;
            lexeme_free(lex);
        } else if (lex->type == LEXTYPE_IDENT) {
            name_t *np = lex->data.ptr;
            size_t len = np->namelen;
            if (len > sizeof(stringresult)-resultlen) {
                len = sizeof(stringresult)-resultlen;
            }
            memcpy(stringresult+resultlen, np->name, len);
            resultlen += len;
            lexeme_free(lex);
        } else {
            /* XXX error condition */
            lexeme_free(lex);
            if (ql_was_normal) {
                pctx->quotelevel = QL_NORMAL;
            }
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return &errlex;
        }
        // OK, now we expect a comma or closing paren
        lex = parser_next(pctx);
        if (lex->type == LEXTYPE_DELIM_RPAR) {
            lexeme_free(lex);
            break;
        }
        if (lex->type != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            lexeme_free(lex);
            if (ql_was_normal) {
                pctx->quotelevel = QL_NORMAL;
            }
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return &errlex;
        }
        // otherwise, continue
    }

    if (ql_was_normal) {
        pctx->quotelevel = QL_NORMAL;
    }
    return lexeme_create(pctx->lexctx, pctx->curscope, LEXTYPE_STRING,
                        stringresult, resultlen);
} /* string_params */

/*
 * %STRING(#p...)
 *
 * Forms a single string by concatenating the parameters.
 */
static int
parse_STRING (parse_ctx_t pctx)
{
    lexeme_t *lex = string_params(pctx, 0);

    lexer_insert(pctx->lexctx, lex);

    return 1;

} /* parse_STRING */

/*
 * %EXACTSTRING(n, fill, #p...)
 *
 * Returns a string that is exactly 'n' characters,
 * either truncated, or filled with the 'fill' character.
 * If only two parameters given, return null string.
 */
static int
parse_EXACTSTRING (parse_ctx_t pctx)
{
    lexeme_t *lex;
    char fillchr = 0;
    size_t len = 0;

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);
    if (parse_Expression(pctx)) {
        lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    } else {
        /* XXX error condition */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (lex->type != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    len = lex->data.val_signed;
    lexeme_free(lex);
    if (len > 1024) {
        /* XXX error condition - string too long */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_COMMA) {
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (parse_Expression(pctx)) {
        lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    } else {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (lex->type != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    fillchr = lex->data.val_signed & 0xff;
    if (lex->data.val_signed > 255) {
        /* XXX error condition - invalid char */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lex = parser_next(pctx);
    if (lex->type == LEXTYPE_DELIM_RPAR) {
        lex->type = LEXTYPE_STRING;
        INITSTR(lex->data.val_string, "", 0);
        lexer_insert(pctx->lexctx, lex);
        return 1;
    } else if (lex->type != LEXTYPE_DELIM_COMMA) {
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lex = string_params(pctx, 1);
    if (lex->type != LEXTYPE_STRING) {
        /* XXX error condition */
        lexeme_free(lex);
        return 1;
    }

    if (lex->data.val_string.len < len) {
        strdesc_t *filldsc;
        filldsc = string_alloc(0, len - lex->data.val_string.len);
        if (filldsc == 0) {
            /* XXX error condition */
        } else {
            memset(filldsc->ptr, fillchr, filldsc->len);
            if (string_append(&lex->data.val_string, filldsc) == 0) {
                /* XXX error condition */
            }
            string_free(filldsc);
        }
    } else if (lex->data.val_string.len > len) {
        lex->data.val_string.len = len;
    }

    lexer_insert(pctx->lexctx, lex);

    return 1;

} /* parse_EXACTSTRING */

/*
 * %CHARCOUNT(#p...)
 *
 * Forms a single string by concatenating the parameters and
 * returns the length in bytes.
 */
static int
parse_CHARCOUNT (parse_ctx_t pctx)
{
    lexeme_t *lex = string_params(pctx, 0);

    if (lex->type != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
    } else {
        size_t len = lex->data.val_string.len;
        lex->type = LEXTYPE_NUMERIC;
        string_free(&lex->data.val_string);
        lex->data.val_signed = len;
        lexer_insert(pctx->lexctx, lex);
    }
    return 1;

} /* parse_CHARCOUNT */

/*
 * %CHAR(code,...)
 *
 * Return a quoted string that consists of the specified
 * characters.  Each 'code' must be a compile-time constant
 * expression in the range 0-255.
 */
static int
parse_CHAR (parse_ctx_t pctx)
{
    lexeme_t *lex;
    int skip_to_paren = 0;
    int hit_error = 0;
    strdesc_t chdsc;
    strdesc_t *result;
    char ch;

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);

    result = string_alloc(0, 0);
    INITSTR(chdsc, &ch, 1);
    while (1) {
        if (!parse_Expression(pctx)) {
            hit_error = 1;
            break;
        }
        lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
        if (lex->type != LEXTYPE_NUMERIC) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        if (lex->data.val_signed > 255) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        ch = lex->data.val_signed & 0xff;
        result = string_append(result, &chdsc);
        lexeme_free(lex);
        lex = parser_next(pctx);
        if (lex == 0 || (lex->type != LEXTYPE_DELIM_COMMA
                         && lex->type != LEXTYPE_DELIM_RPAR)) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        if (lex->type == LEXTYPE_DELIM_RPAR) {
            lexeme_free(lex);
            lex = 0;
            break;
        }
    }

    if (hit_error) {
        string_free(result);
        if (skip_to_paren) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        }
    } else {
        lex = lexeme_alloc(LEXTYPE_STRING);
        memcpy(&lex->data.val_string, result, sizeof(strdesc_t));
        lexer_insert(pctx->lexctx, lex);
    }
    
    return 1;

} /* parse_CHAR */

/*
 * %EXPLODE(#p,...)
 * 
 * Return a sequence of quoted-string lexemes, one for each
 * character in the string(s) specified as parameters.
 * An empty parameter sequence results in a null string.
 */
static int
parse_EXPLODE (parse_ctx_t pctx)
{
    lexeme_t *lex = string_params(pctx, 0);
    char *cp;
    size_t remain;
    lexeme_t *result = 0, *last = 0;

    if (lex->type != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }

    if (lex->data.val_string.len == 0) {
        result = lexeme_alloc(LEXTYPE_STRING);
        INITSTR(result->data.val_string, "", 0);
        lexer_insert(pctx->lexctx, result);
        return 1;
    }

    for (cp = lex->data.val_string.ptr, remain = lex->data.val_string.len;
         remain > 0; cp += 1, remain -= 1) {
        lexeme_t *clex = lexeme_alloc(LEXTYPE_STRING);
        string_from_chrs(&clex->data.val_string, cp, 1);
        if (result == 0) {
            result = last = clex;
        } else {
            last->next = clex;
            last = clex;
        }
        if (remain > 1) {
            clex = lexeme_alloc(LEXTYPE_DELIM_COMMA);
            last->next = clex;
            last = clex;
        }
    }
    if (result != 0) {
        lexer_insert(pctx->lexctx, result);
    }
    return 1;

} /* parse_EXPLODE */

/*
 * parse_lexeme_seq
 *
 * Parses an arbitrary sequence of lexemes until hitting
 * a specified delimiter, returning the sequence as a chain.
 * Handles parentheses, brackets, etc.
 */
static int
parse_lexeme_seq (parse_ctx_t pctx, lextype_t terms[],
                  int nterms, lexeme_t **chain, lextype_t *term)
{
    lexeme_t *lex;
    lexeme_t *result, *last;
    int i;
    int depth[3], status, hit_term;

    *chain = 0;
    result = last = 0;
    depth[0] = depth[1] = depth[2] = 0;
    status = 1;
    hit_term = 0;

    while (status) {
        lex = parser_next(pctx);
        if (lex->type == LEXTYPE_END) {
            lexer_insert(pctx->lexctx, lex);
            status = 0;
            break;
        }
        for (i = 0; i < nterms; i++) {
            if (lex->type == terms[i] && depth[0] == 0 &&
                depth[1] == 0 && depth[2] == 0) {
                hit_term = 1;
                break;
            }
        }
        if (hit_term) {
            if (term != 0) *term = lex->type;
            lexeme_free(lex);
            break;
        }
        for (i = 0; i < 3; i++) {
            if (lex->type == opener[i]) {
                depth[i]+= 1;
                break;
            } else if (lex->type == closer[i]) {
                if (depth[i] > 0) {
                    depth[i] -= 1;
                } else {
                    status = 0;
                }
                break;
            }
        }
        if (result == 0) {
            result = last = lex;
        } else {
            last->next = lex;
            last = lex;
        }
    }

    *chain = result;
    return status;

} /* parse_lexeme_seq */

/*
 * %REMOVE(#p)
 * 
 * Removes a matching pair of parentheses, square brackets, or
 * angle brackets from the parameter, if there is such a pair.
 * Otherwise, the parameter remains unchanged.
 */
static int
parse_REMOVE (parse_ctx_t pctx)
{
    lexeme_t *lex;
    lexeme_t *result, *last;
    int return_to_normal = 0;
    int i;
    int depth[3];
    lextype_t rpar[1] = {LEXTYPE_DELIM_RPAR};

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }

    if (pctx->quotelevel == QL_NORMAL) {
        return_to_normal = 1;
        pctx->quotelevel = QL_NAME;
    }

    if (!parse_lexeme_seq(pctx, rpar, 1, &result, 0)) {
        /* XXX error condition */
        lexseq_free(result);
        return 1;
    }

    if (result == 0) {
        return 1;
    }

    for (last = result; last->next != 0; last = last->next);

    // If the chain begins with an opener and ends with
    // its corresponding closer, trim them off before
    // inserting the chain back into the stream.  However,
    // we can't just blindly trim without checking to make
    // sure that they're really enclosing the entire sequence.
    // Otherwise, we'd trim '(A)+(B)', for instance.
    for (i = 0; i < 3; i++) {
        if (result->type == opener[i]) {
            break;
        }
    }
    if (i < 3 && result->next != 0 && last->type == closer[i]) {
        int doit = 1;
        depth[0] = depth[1] = depth[2] = 0;
        for (lex = result->next; lex->next != last; lex = lex->next) {
            for (i = 0; doit && (i < 3); i++) {
                if (lex->type == opener[i]) {
                    depth[i] += 1;
                } else if (lex->type == closer[i]) {
                    if (depth[i] == 0) {
                        doit = 0;
                        break;
                    }
                    depth[i] -= 1;
                }
            }
        }
        // If 'doit' is true, then we haven't found a premature
        // closing of the inner opener, so we should trim.
        if (doit) {
            lex->next = 0; // it's pointing to the new tail right now
            lexeme_free(last);
            lex = result;
            result = result->next;
            lexeme_free(lex);
        }
    }

    lexer_insert(pctx->lexctx, result);
    if (return_to_normal) {
        pctx->quotelevel = QL_NORMAL;
    }

    return 1;

} /* parse_REMOVE */

/*
 * do_name_qname
 *
 * Common code for %NAME and %QUOTENAME.
 *
 * %NAME(#p,...)
 * %QUOTENAME(#p,...)
 *
 * Create a name from an aribtrary string.
 */
static int
do_name_qname (parse_ctx_t pctx, int doquote)
{
    lexeme_t *lex = string_params(pctx, 0);
    lexeme_t *result;

    if (lex->type != LEXTYPE_STRING) {
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    result = lexeme_alloc(LEXTYPE_IDENT);
    result->data.ptr = name_search(pctx->curscope, lex->data.val_string.ptr,
                                   lex->data.val_string.len, 1);
    lexer_insert(pctx->lexctx, result);
    lexeme_free(lex);

    if (doquote) {
        pctx->do_quote = 1;
    }

    return 1;
    
} /* do_name_qname */

static int parse_NAME (parse_ctx_t pctx) { return do_name_qname(pctx, 0); }
static int parse_QUOTENAME (parse_ctx_t pctx) { return do_name_qname(pctx, 1); }

/*
 * %NULL(#p,..)
 *
 * Returns 1 if all of the parameters are null, 0 otherwise.
 */
static int
parse_NULL (parse_ctx_t pctx)
{
    lexeme_t *lex;
    int allnull = 1;
    int return_to_normal = pctx->quotelevel == QL_NORMAL;
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };
    lextype_t which;

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);

    if (return_to_normal) {
        pctx->quotelevel = QL_NAME;
    }

    lex = 0;
    while (1) {
        if (!parse_lexeme_seq(pctx, terms, 2, &lex, &which)) {
            /* XXX error condition */
            lexseq_free(lex);
            if (return_to_normal) {
                pctx->quotelevel = QL_NORMAL;
            }
            return 1;
        }
        if (lex != 0) {
            lexseq_free(lex);
            allnull = 0;
        }
        if (which == LEXTYPE_DELIM_RPAR) {
            break;
        }
    }

    lex = lexeme_alloc(LEXTYPE_NUMERIC);
    lex->data.val_signed = allnull;
    lexer_insert(pctx->lexctx, lex);

    if (return_to_normal) {
        pctx->quotelevel = QL_NORMAL;
    }

    return 1;

} /* parse_NULL */

/*
 * %IDENTICAL(#s1, #s2)
 *
 * Returns 1 if the two sequences of lexemes are
 * identical.
 */
static int
parse_IDENTICAL (parse_ctx_t pctx)
{
    lexeme_t *lex;
    int return_to_normal = pctx->quotelevel == QL_NORMAL;
    lexeme_t *chain[2];
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);
    if (return_to_normal) {
        pctx->quotelevel = QL_NAME;
    }

    chain[0] = chain[1] = 0;
    if (parse_lexeme_seq(pctx, &terms[0], 1, &chain[0], 0) &&
        parse_lexeme_seq(pctx, &terms[1], 1, &chain[1], 0)) {
        lex = lexeme_alloc(LEXTYPE_NUMERIC);
        lex->data.val_signed = lexemes_match(chain[0], chain[1]);
        lexer_insert(pctx->lexctx, lex);
    } else {
        /* XXX error condition */
    }

    lexseq_free(chain[0]);
    lexseq_free(chain[1]);

    if (return_to_normal) {
        pctx->quotelevel = QL_NORMAL;
    }
    return 1;

} /* parse_IDENTICAL */

/*
 * %ISSTRING(exp,...)
 *
 * Returns 1 if every expression results in a quoted-string
 * lexeme, otherwise 0.
 */
static int
parse_ISSTRING (parse_ctx_t pctx)
{
    lexeme_t *lex;
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };
    lextype_t which;
    int allstr = 1;

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);
    lex = 0;

    while (1) {
        if (!parse_lexeme_seq(pctx, terms, 2, &lex, &which)) {
            /* XXX error condition */
            lexseq_free(lex);
            return 1;
        }
        if (lex == 0 || (lex->type != LEXTYPE_STRING &&
                         lex->type != LEXTYPE_CSTRING) || lex->next != 0) {
            allstr = 0;
        }
        lexseq_free(lex);
        if (which == LEXTYPE_DELIM_RPAR) {
            break;
        }
    }
    lex = lexeme_alloc(LEXTYPE_NUMERIC);
    lex->data.val_signed = allstr;
    lexer_insert(pctx->lexctx, lex);

    return 1;
}

/*
 * %IF lexical-test %THEN ... [ %ELSE ... ] %FI
 *
 * Lexical conditional processing.
 *  - macros using %IF must have a fully formed sequence
 *  - end-of-file not permitted in the middle of this sequence
 *  - must handle nested sequences!!
 *  - The test is TRUE only if the *** low-order bit *** is 1
 */
static int
parse_IF (parse_ctx_t pctx)
{
    lexeme_t *lex;

    lex = parser_next(pctx);
    if (lex->type != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lex = parser_next(pctx);
    return 1;
}