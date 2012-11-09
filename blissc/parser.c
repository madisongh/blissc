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

typedef enum {
    COND_NORMAL = 0,
    COND_CWC, COND_CWA,
    COND_AWC, COND_AWA
} condstate_t;

struct parse_ctx_s {
    scopectx_t      kwdscope, curscope;
    void            *cctx;
    lexer_ctx_t     lexctx;
    int             lib_compile;
    quotelevel_t    quotelevel;
    condstate_t     condstate[64];
    int             condlevel;
    int             no_eof;
    int             do_quote, do_unquote;
    int             declarations_ok;
};

static int macro_expand(parse_ctx_t pctx, name_t *macroname);

#undef DODEF
#define DODEFS \
    DODEF(ASCII, 0) DODEF(ASCIC, 0) DODEF(ASCIZ, 0) \
    DODEF(B, 0) DODEF(O, 0) DODEF(DECIMAL, 0) DODEF(X, 0) DODEF(C, 0) \
    DODEF(STRING, 0) DODEF(EXACTSTRING, 0) DODEF(CHARCOUNT, 0) DODEF(CHAR, 0) \
    DODEF(EXPLODE, 0) DODEF(REMOVE, 0) \
    DODEF(NAME, 0) DODEF(QUOTENAME, 0) \
    DODEF(NULL, 0) DODEF(IDENTICAL, 0) \
    DODEF(ISSTRING, 0) DODEF(REQUIRE, 0) \
    DODEF(QUOTE, NAME_M_QFUNC) DODEF(UNQUOTE, NAME_M_QFUNC) DODEF(EXPAND, NAME_M_QFUNC) \
    DODEF(IF, NAME_M_NOQUOTE|NAME_M_IS_PCTIF) DODEF(ELSE, NAME_M_NOQUOTE) \
    DODEF(FI, NAME_M_NOQUOTE)

#define DODEF(name_, flags_) static int parse_##name_ (parse_ctx_t);
DODEFS
#undef DODEF
#define DODEF(name_, flags_) LEXDEF("%" #name_, parse_##name_, flags_),

static name_t parser_names[] = {
    DODEFS
    LEXDEF("%THEN", LEXTYPE_DELIM_PTHEN, NAME_M_OPERATOR|NAME_M_NOQUOTE),
};
#undef DODEFS
#undef DODEF

static lexeme_t errlex = { 0, LEXTYPE_NONE };

static lextype_t opener[3] = {
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_LBRACK };
static lextype_t closer[3] = {
    LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RANGLE, LEXTYPE_DELIM_RBRACK };

static strdesc_t nullstr = STRDEF(""), one = STRDEF("1"), zero = STRDEF("0");


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

    pctx->declarations_ok = 1; /* XXX for now */

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
 * COMPILETIME variables) to their values, the current lexical
 * conditional state, expanding macros, and dispatching to
 * process keywords and lexical functions.
 */
lextype_t
parser_next (parse_ctx_t pctx, lexeme_t **lexp)
{
    lexeme_t *lex;
    lextype_t lt;
    name_t *np;
    int keepgoing = 1;
    int do_bind, do_expand;
    parser_dispatch_t doit;
    int cond_skip;

    while (keepgoing) {
        lex = lexer_next(pctx->lexctx, pctx->curscope, pctx->no_eof);
        lt = lexeme_boundtype(lex);
        keepgoing = 0;
        if (lex->type == LEXTYPE_NONE || lex->type == LEXTYPE_END) {
            break;
        }
        cond_skip = (pctx->condstate[pctx->condlevel] == COND_AWC ||
                     pctx->condstate[pctx->condlevel] == COND_CWA);
        if (pctx->do_quote) {
            pctx->do_quote = pctx->do_unquote = 0;
            if (cond_skip) {
                lexeme_free(lex);
                keepgoing = 1;
                continue;
            }
            lt = LEXTYPE_TEXT;
            break;
        }

        if (lt == LEXTYPE_IDENT) {
            np = lexeme_nameval(lex, pctx->curscope);
        }
        do_bind = do_expand = 0;
        switch (pctx->quotelevel) {
            case QL_MACRO:
                do_bind = pctx->do_unquote ||
                    (lt == LEXTYPE_IDENT && np->nametype == NAMETYPE_MAC_PARAM);
                do_expand = pctx->do_unquote == 2 || (lt == LEXTYPE_IDENT &&
                            (np->nametype == NAMETYPE_LEXFUNC &&
                              (np->nameflags & NAME_M_QFUNC) != 0));
                break;
            case QL_NAME:
                do_bind = pctx->do_unquote || lt != LEXTYPE_IDENT ||
                    (lt == LEXTYPE_IDENT && np->nametype == NAMETYPE_MACRO);
                do_expand = pctx->do_unquote == 2 ||
                    (lt == LEXTYPE_IDENT &&
                            (np->nametype == NAMETYPE_LEXFUNC ||
                             np->nametype == NAMETYPE_MACRO));
                break;
            case QL_NORMAL:
                do_bind = do_expand = 1;
                break;
        }

        pctx->do_quote = pctx->do_unquote = 0;
        if (!do_bind && !do_expand) {
            if (cond_skip) {
                lexeme_free(lex);
                keepgoing = 1;
                continue;
            }
            lt = LEXTYPE_TEXT;
            break;
        }

        if (lt == LEXTYPE_IDENT) {
            // Make sure we handle %ELSE and %FI (but not %IF) if
            // we would otherwise be skipping over lexemes in a
            // lexical conditional.
            if (cond_skip &&
                ((np->nameflags & (NAME_M_NOQUOTE|NAME_M_IS_PCTIF)) == NAME_M_NOQUOTE)) {
                cond_skip = 0;
            }
        }
        if (cond_skip) {
            lexeme_free(lex);
            keepgoing = 1;
            continue;
        }
        lex = lexeme_bind(pctx->curscope, lex);
        lt = lexeme_boundtype(lex);
        if (lt == LEXTYPE_IDENT) {
            np = lexeme_nameval(lex, pctx->curscope);
            if ((do_bind && np->nametype == NAMETYPE_KEYWORD) ||
                (do_expand && np->nametype == NAMETYPE_LEXFUNC)) {
                if (np->nameflags & NAME_M_OPERATOR) {
                    strdesc_t dsc;
                    lexeme_free(lex);
                    strdesc_init(&dsc, np->name, np->namelen);
                    lt = (lextype_t) np->namedata.val_signed;
                    lex = lexeme_create(lt, &dsc);
                    keepgoing = 0;
                    break;
                }
                doit = np->namedata.ptr;
                keepgoing = doit(pctx);
                lexeme_free(lex);
                lex = &errlex;
                continue;
            }
            if (np->nametype == NAMETYPE_MACRO) {
                if (do_expand) {
                    keepgoing = macro_expand(pctx, np);
                    lexeme_free(lex);
                    lex = &errlex;
                    continue;
                }
            }
        }
        break;

    } /* while keepgoing */

    *lexp = lex;
    return lt;

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
    lextype_t lt;

    for (lt = parser_next(pctx, &lex); lt != LEXTYPE_END &&
         lt != LEXTYPE_NONE && lt != delimtype;
         lt = parser_next(pctx, &lex)) {
        lexeme_free(lex);
    }

    lexeme_free(lex);
}

/*
 * parser_decl_ok
 *
 * Returns 1 if declarations are OK in the current
 * parse context (between start of a block and any
 * normal expressions).  Otherwise, 0.
 */
int
parser_decl_ok (parse_ctx_t pctx, scopectx_t *curscope)
{
    if (pctx->declarations_ok && curscope != 0) {
        *curscope = pctx->curscope;
    }
    return pctx->declarations_ok;

} /* parser_decl_ok */

/*
 * parser_get_quotelevel
 *
 * Return the current quotelevel.
 */
quotelevel_t
parser_get_quotelevel (parse_ctx_t pctx)
{
    return pctx->quotelevel;

} /* parser_get_quotelevel */

/*
 * parser_set_quotelevel
 *
 * Set the current quotelevel, returning the
 * current value.
 */
quotelevel_t
parser_set_quotelevel (parse_ctx_t pctx, quotelevel_t ql)
{
    quotelevel_t oldql = pctx->quotelevel;
    pctx->quotelevel = ql;
    return oldql;
}

/*
 * parser_incr/decr_erroneof
 *
 * Bump up/down the erroneof setting.
 */
void parser_incr_erroneof (parse_ctx_t pctx) { pctx->no_eof += 1; }
void parser_decr_erroneof (parse_ctx_t pctx) { pctx->no_eof -= 1; }

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
    lextype_t lt;

    if (pctx->quotelevel < QL_NAME) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    lt = lexeme_boundtype(lex);
    // %QUOTE only applies to names, lexical function names,
    // commas, and percent signs.
    if (lt != LEXTYPE_IDENT &&
        lt != LEXTYPE_DELIM_COMMA &&
        lt != LEXTYPE_DELIM_PERCENT) {
        /* XXX error condition */
    } else if (lt == LEXTYPE_IDENT) {
        name_t *np = lexeme_nameval(lex, pctx->curscope);
        if (np->nametype == NAMETYPE_KEYWORD ||
            (np->nameflags & NAME_M_NOQUOTE) != 0) {
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
    lextype_t lt;

    if (pctx->quotelevel < (is_expand ? QL_MACRO : QL_NAME)) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_next(pctx->lexctx, pctx->curscope, 0);
    lt = lexeme_boundtype(lex);
    if (lt != LEXTYPE_IDENT) {
        /* XXX error condition */
    } else {
        name_t *np = lexeme_nameval(lex, pctx->curscope);
        if (np->nametype == NAMETYPE_KEYWORD ||
            np->nametype == NAMETYPE_UNDECLARED ||
            (is_expand && !((np->nametype == NAMETYPE_LEXFUNC) ||
                            (np->nametype == NAMETYPE_MACRO)))) {
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
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str = lexeme_stringval(lex);

    if (lt != LEXTYPE_STRING || (whichtype == 2 && str->len > 255)) {
        /* XXX error condition */
        string_free(str);
        lexeme_free(lex);
    } else {
        lexeme_t *newlex;
        if (whichtype == 1) {
            static strdesc_t nullchr = STRZDEF("");
            str = string_append(str, &nullchr);
        } else if (whichtype == 2) {
            lt = LEXTYPE_CSTRING;
        }
        newlex = lexeme_create(lt, str);
        lexer_insert(pctx->lexctx, newlex);
        lexeme_free(lex);
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
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str;
    char buf[32], *cp;
    long numval;

    if (lt != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    str = lexeme_stringval(lex);
    cp = buf + (str->len >= sizeof(buf) ? sizeof(buf)-1 : str->len);
    // XXX probably need to do this ourselves, rather than rely on C lib
    memcpy(buf, str->ptr, cp-buf);
    *cp = '\0';
    errno = 0;
    numval = strtol(buf, &cp, base);
    if (errno != 0 || (cp-buf) != str->len) {
        /* XXX error condition */
    } else {
        // XXX need to validate that the value fits into the
        // target machine's word length
        strdesc_t dsc;
        int len = snprintf(buf, sizeof(buf), "%ld", numval);
        strdesc_init(&dsc, buf, len);
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_NUMERIC, &dsc));
    }
    string_free(str);
    lexeme_free(lex);

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
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str = lexeme_stringval(lex);
    if (lt != LEXTYPE_STRING || str->len != 1) {
        /* XXX error condition */
    } else {
        char buf[8];
        int len = snprintf(buf, sizeof(buf), "%ld", (long)(*str->ptr & 0x7f));
        strdesc_t dsc;
        strdesc_init(&dsc, buf, len);
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_NUMERIC, &dsc));
    }
    string_free(str);
    lexeme_free(lex);

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
    lextype_t lt;
    int       ql_was_normal = 0;
    strdesc_t *result = string_alloc(0,0);

    if (!already_have_open_paren) {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_DELIM_LPAR) {
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
        lt = parser_next(pctx, &lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            lexeme_free(lex);
            break;
        }
        // It's OK to have a null argument
        if (lt == LEXTYPE_DELIM_COMMA) {
            lexeme_free(lex);
            continue;
        }
        switch (lt) {
            case LEXTYPE_NUMERIC:
            case LEXTYPE_CSTRING:
            case LEXTYPE_STRING:
            case LEXTYPE_IDENT:
                result = string_append(result, &lex->text);
                break;
            default:
                lexeme_free(lex);
                if (ql_was_normal) {
                    pctx->quotelevel = QL_NORMAL;
                }
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
                return &errlex;
        }
        // OK, now we expect a comma or closing paren
        lt = parser_next(pctx, &lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            lexeme_free(lex);
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
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
    lex = lexeme_create(LEXTYPE_STRING, result);
    string_free(result);
    return lex;

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
    lextype_t lt;
    char fillchr = 0;
    size_t len = 0;
    strdesc_t *str;

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);
    if (parse_Expression(pctx)) {
        lt = parser_next(pctx, &lex);
    } else {
        /* XXX error condition */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (lt != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    len = lexeme_unsignedval(lex);
    lexeme_free(lex);
    if (len > 1024) {
        /* XXX error condition - string too long */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_COMMA) {
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (parse_Expression(pctx)) {
        lt = parser_next(pctx, &lex);
    } else {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (lt != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    fillchr = lexeme_unsignedval(lex) & 0xff;
    if (lexeme_signedval(lex) > 255) {
        /* XXX error condition - invalid char */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lexeme_free(lex);
    lt = parser_next(pctx, &lex);
    lexeme_free(lex);
    if (lt == LEXTYPE_DELIM_RPAR) {
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_STRING, &nullstr));
        return 1;
    } else if (lt != LEXTYPE_DELIM_COMMA) {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lex = string_params(pctx, 1);
    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        /* XXX error condition */
        lexeme_free(lex);
        return 1;
    }
    str = lexeme_stringval(lex);
    if (str->len < len) {
        strdesc_t *filldsc;
        filldsc = string_alloc(0, len - lex->data.val_string.len);
        if (filldsc == 0) {
            /* XXX error condition */
        } else {
            memset(filldsc->ptr, fillchr, filldsc->len);
            str = string_append(str, filldsc);
            if (str == 0) {
                /* XXX error condition */
            }
            string_free(filldsc);
        }
    } else if (str->len > len) {
        str->len = len;
    }

    lexeme_free(lex);
    lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_STRING, str));
    string_free(str);

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
    strdesc_t *str, dsc;
    char buf[8];
    int len;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    str = lexeme_stringval(lex);
    len = snprintf(buf, sizeof(buf), "%d", str->len);
    strdesc_init(&dsc, buf, len);
    lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_NUMERIC, &dsc));
    lexeme_free(lex);
    string_free(str);
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
    lextype_t lt;
    int skip_to_paren = 0;
    int hit_error = 0;
    strdesc_t chdsc;
    strdesc_t *result;
    char ch;
    long val;

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
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
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_NUMERIC) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        val = lexeme_signedval(lex);
        if (val > 255) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        ch = val & 0xff;
        result = string_append(result, &chdsc);
        lexeme_free(lex);
        lt = parser_next(pctx, &lex);
        lexeme_free(lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
    }

    if (hit_error) {
        if (skip_to_paren) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        }
    } else {
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_STRING, result));
    }
    string_free(result);

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
    strdesc_t *str, dsc;
    size_t remain;
    lexeme_t *result = 0, *last = 0;
    static strdesc_t comma = STRDEF(",");

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }

    str = lexeme_stringval(lex);
    if (str->len == 0) {
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_STRING, str));
        string_free(str);
        lexeme_free(lex);
        return 1;
    }
    INITSTR(dsc, str->ptr, 1);
    for (remain = str->len; remain > 0; dsc.ptr += 1, remain -= 1) {
        lexeme_t *clex = lexeme_create(LEXTYPE_STRING, &dsc);
        if (result == 0) {
            result = last = clex;
        } else {
            last->next = clex;
            last = clex;
        }
        if (remain > 1) {
            lexeme_t *clex = lexeme_create(LEXTYPE_DELIM_COMMA, &comma);
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
    lextype_t lt;
    lexeme_t *result, *last;
    int i;
    int depth[3], status, hit_term;

    *chain = 0;
    result = last = 0;
    depth[0] = depth[1] = depth[2] = 0;
    status = 1;
    hit_term = 0;

    while (status) {
        lt = parser_next(pctx, &lex);
        if (lt == LEXTYPE_END) {
            lexer_insert(pctx->lexctx, lex);
            status = 0;
            break;
        }
        for (i = 0; i < nterms; i++) {
            if (lt == terms[i] && depth[0] == 0 &&
                depth[1] == 0 && depth[2] == 0) {
                hit_term = 1;
                break;
            }
        }
        if (hit_term) {
            if (term != 0) *term = lt;
            lexeme_free(lex);
            break;
        }
        for (i = 0; i < 3; i++) {
            if (lt == opener[i]) {
                depth[i]+= 1;
                break;
            } else if (lt == closer[i]) {
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
    lextype_t lt;
    int return_to_normal = 0;
    int i;
    int depth[3];
    lextype_t rpar[1] = {LEXTYPE_DELIM_RPAR};

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
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
        if (return_to_normal) {
            pctx->quotelevel = QL_NORMAL;
        }
        return 1;
    }

    if (return_to_normal) {
        pctx->quotelevel = QL_NORMAL;
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

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    result = lexeme_create(LEXTYPE_IDENT, lexeme_stringval(lex));
    lexer_insert(pctx->lexctx, result);
    lexeme_free(lex);

    if (doquote) {
        name_t *np = lexeme_nameval(lex, pctx->curscope);
        if (np->nameflags & NAME_M_NOQUOTE) {
            /* XXX error condition */
        } else {
            pctx->do_quote = 1;
        }
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
    lextype_t lt;
    int allnull = 1;
    int return_to_normal = pctx->quotelevel == QL_NORMAL;
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };
    lextype_t which;

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);

    if (return_to_normal) {
        pctx->quotelevel = QL_NAME;
    }

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

    lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_NUMERIC, (allnull ? &one : &zero)));

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
    lextype_t lt;
    int return_to_normal = pctx->quotelevel == QL_NORMAL;
    lexeme_t *chain[2];
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
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
        lexer_insert(pctx->lexctx,
                     lexeme_create(LEXTYPE_NUMERIC,
                        (lexemes_match(chain[0], chain[1]) ? &one : &zero)));
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
    lextype_t lt;
    int allstr = 1;
    int hit_error = 0;

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    lexeme_free(lex);

    while (1) {
        if (!parse_Expression(pctx)) {
            /* XXX error condition */
            hit_error = 1;
            break;
        }
        lt = parser_next(pctx, &lex);
        lexeme_free(lex);
        if (lt == LEXTYPE_END || lt == LEXTYPE_NONE) {
            /* XXX error condition */
            hit_error = 1;
            break;
        }
        if (lt != LEXTYPE_STRING && lt != LEXTYPE_CSTRING) {
            allstr = 0;
        }
        if (lt != LEXTYPE_DELIM_RPAR && lt != LEXTYPE_DELIM_COMMA) {
            lt = parser_next(pctx, &lex);
            lexeme_free(lex);
        }
        if (lt == LEXTYPE_DELIM_RPAR) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            /* XXX error condition */
            hit_error = 1;
            break;
        }
    }

    if (!hit_error) {
        lexer_insert(pctx->lexctx, lexeme_create(LEXTYPE_NUMERIC,
                                                 (allstr ? &one : &zero)));
    }

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
 *
 * We use a stack of state variables to track our current lexical-conditional
 * state.  State values are:
 *
 * COND_NORMAL - not in a lexical conditional
 * COND_CWA    - in a consequent (%THEN sequence), but want alternative
 * COND_CWC    - in a consequent, and want the consequent
 * COND_AWA    - in an alternative (%ELSE sequence), and want it
 * COND_AWC    - in an alternative, but want the consequent
 *
 * parser_next() ignores lexemes other than %ELSE and %FI while our
 * current state is CWA or AWC.
 *
 * If we encounter a new %IF while in a state other than COND_NORMAL,
 * the current state is stacked and we move to a new condlevel.
 */
static int
parse_IF (parse_ctx_t pctx)
{
    lexeme_t *lex;
    lextype_t lt;
    int testval;

    // Note the use of increment/decrement here -- this is to handle
    // nesting of instances in which hitting end-of-file is a no-no.
    pctx->no_eof += 1;

    if (!parse_Expression(pctx)) {
        /* XXX error condition */
        pctx->no_eof -= 1;
        return 1;
    }

    // if the expression is compile-time constant,
    // we will have a numeric literal next in the stream.
    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_NUMERIC) {
        /* XXX error condition */
        lexeme_free(lex);
        pctx->no_eof -= 1;
        return 1;
    }

    testval = lexeme_signedval(lex) & 1;
    lexeme_free(lex);

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_PTHEN) {
        /* XXX error condition */
        lexer_insert(pctx->lexctx, lex);
        pctx->no_eof -= 1;
        return 1;
    }
    lexeme_free(lex);

    if (pctx->condstate[pctx->condlevel] != COND_NORMAL) {
        if (pctx->condlevel >= sizeof(pctx->condstate)/sizeof(pctx->condstate[0])) {
            /* XXX error condition */
            pctx->no_eof -= 1;
            return 0;
        }
        pctx->condlevel += 1;
    }
    pctx->condstate[pctx->condlevel] = (testval ? COND_CWC : COND_CWA);
    return 1;

} /* parse_IF */

/*
 * %ELSE
 *
 * If we're in a consequent (COND_CWx state), move to alternative
 * state (COND_AWx state).  Otherwise, we have an error.
 */
static int
parse_ELSE (parse_ctx_t pctx)
{
    condstate_t curstate = pctx->condstate[pctx->condlevel];
    if (curstate != COND_CWA && curstate != COND_CWC) {
        /* XXX error condition */
        return 1;
    }
    pctx->condstate[pctx->condlevel] = (curstate == COND_CWA ? COND_AWA : COND_AWC);
    return 1;

} /* parse_ELSE */

/*
 * %FI
 *
 * Terminate a lexical-conditional expression.  If we aren't currently
 * in a lexical conditional (state is COND_NORMAL), that's an error.
 *
 * If condlevel is non-zero, we're in a nested conditional, so we pop
 * the condlevel stack.
 */
static int
parse_FI (parse_ctx_t pctx)
{
    condstate_t curstate = pctx->condstate[pctx->condlevel];
    if (curstate == COND_NORMAL) {
        /* XXX error condition */
        return 1;
    }
    pctx->condstate[pctx->condlevel] = COND_NORMAL;
    if (pctx->condlevel > 0) {
        pctx->condlevel -= 1;
    }
    pctx->no_eof -= 1;
    return 1;

} /* parse_FI */

/*
 * %REQUIRE(#p,...)
 *
 * The parameters are treated as %STRING(...) parameters.  The
 * resulting string is used as a file name, and that file is
 * inserted into the lexeme stream.
 */
static int
parse_REQUIRE (parse_ctx_t pctx)
{
    lexeme_t *lex = string_params(pctx, 0);
    strdesc_t *str;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        /* XXX - error condition */
        lexeme_free(lex);
        return 1;
    }
    str = lexeme_stringval(lex);
    if (!lexer_newfile(pctx->lexctx, str->ptr, str->len)) {
        /* XXX - error condition */
    }
    lexeme_free(lex);
    string_free(str);
    return 1;
}
