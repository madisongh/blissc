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
#include "macros.h"

struct parse_ctx_s {
    scopectx_t      kwdscope, curscope;
    void            *cctx;
    lexer_ctx_t     lexctx;
    int             lib_compile;
    quotelevel_t    quotelevel;
    quotemodifier_t quotemodifier;
    condstate_t     condstate[64];
    int             condlevel;
    int             no_eof;
    int             declarations_ok;
    int             fileno;
    unsigned int    lineno, colno;
};

typedef int (*lexfunc_t)(parse_ctx_t pctx, lextype_t curlt);

#undef DODEF
#define DODEFS \
    DODEF(ASCII, parse_string_literal) \
    DODEF(ASCIC, parse_string_literal) \
    DODEF(ASCIZ, parse_string_literal) \
    DODEF(B, parse_numeric_literal) \
    DODEF(O, parse_numeric_literal) \
    DODEF(DECIMAL, parse_numeric_literal) \
    DODEF(X, parse_numeric_literal) DODEF(C, parse_C) \
    DODEF(STRING, parse_STRING) DODEF(EXACTSTRING, parse_EXACTSTRING) \
    DODEF(CHARCOUNT, parse_CHARCOUNT) DODEF(CHAR, parse_CHAR) \
    DODEF(EXPLODE, parse_EXPLODE) DODEF(REMOVE, parse_REMOVE) \
    DODEF(NAME, parse_name_qname) DODEF(QUOTENAME, parse_name_qname) \
    DODEF(NULL, parse_NULL) DODEF(IDENTICAL, parse_IDENTICAL) \
    DODEF(ISSTRING, parse_ISSTRING) DODEF(REQUIRE, parse_REQUIRE) \
    DODEF(QUOTE, parse_QUOTE) \
    DODEF(UNQUOTE, parse_unquote_expand) \
    DODEF(EXPAND, parse_unquote_expand) \
    DODEF(IF, parse_IF) DODEF(ELSE, parse_ELSE) DODEF(FI, parse_FI) \
    DODEF(ASSIGN, parse_ASSIGN)

#define DODEF(name_, rtn_) static int rtn_ (parse_ctx_t, lextype_t);
DODEFS
#undef DODEF
#define DODEF(name_, rtn_) [LEXTYPE_LXF_##name_-LEXTYPE_LXF_MIN] = rtn_,

static lexfunc_t lexfuncs[LEXTYPE_LXF_MAX-LEXTYPE_LXF_MIN+1] = {
    DODEFS
};
#undef DODEF

#define DODEF(name_, rtn_) NAMEDEF("%" #name_, LEXTYPE_LXF_##name_, NAME_M_RESERVED),

static name_t parser_names[] = {
    DODEFS
    NAMEDEF("%THEN", LEXTYPE_LXF_THEN, NAME_M_RESERVED),
};
#undef DODEF

static lexeme_t errlex = { 0, LEXTYPE_NONE };

static lextype_t opener[3] = {
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_LBRACK };
static lextype_t closer[3] = {
    LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RANGLE, LEXTYPE_DELIM_RBRACK };

static strdesc_t nullstr = STRDEF(""), one = STRDEF("1"), zero = STRDEF("0");

static int is_lexfunc (lextype_t lt, int exclude_qfuncs) {
    if (lt < LEXTYPE_LXF_MIN || lt > LEXTYPE_LXF_MAX) return 0;
    if (!exclude_qfuncs) return 1;
    return (lt < LEXTYPE_LXF_QUOTE || lt > LEXTYPE_LXF_EXPAND);
}
static int is_operator(lextype_t lt) {
    return (lt >= LEXTYPE_OP_MIN && lt <= LEXTYPE_OP_MAX);
}
static int is_name(lextype_t lt) {
    return (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX);
}

static lexeme_t *
parser_lexeme_create (parse_ctx_t pctx, lextype_t lt, strdesc_t *text) {
    lexeme_t *lex = lexeme_create(lt, text);
    lexeme_setpos(lex, pctx->fileno, pctx->lineno, pctx->colno);
    return lex;
}
/*
 * name_bind
 *
 * Binds a lexeme of LEXTYPE_NAME to its actual name type,
 * by performing a name lookup.
 *
 * Returns:
 *  -1: error
 *   0: reuse lexeme
 *   1: lexeme freed; use the result sequence
 */
static int
name_bind (void *ctx, quotelevel_t ql, quotemodifier_t qm,
              lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    parse_ctx_t pctx = ctx;
    strdesc_t *ltext = lexeme_text(lex);
    name_t *np = name_search(pctx->curscope, ltext->ptr, ltext->len, 1);
    lextype_t nt;

    if (np == 0) {
        // XXX error condition
        return -1;
    }

    nt = name_type(np);

    // Check for lexical conditional skips
    if (cs == COND_CWA) {
        if (nt != LEXTYPE_LXF_ELSE && nt != LEXTYPE_LXF_FI) {
            lexeme_free(lex);
            return 1;
        }
    } else if (cs == COND_AWC) {
        if (nt != LEXTYPE_LXF_FI) {
            lexeme_free(lex);
            return 1;
        }
    }

    lex->boundtype = nt;
    lexeme_ctx_set(lex, np);
    if (nt == LEXTYPE_NAME) {
        // undeclared name, no type known
        lex->type = nt;
        return 0;
    }

    // Now that we've resolved the name, repeat the
    // binding process on the resulting lexeme.

    return lexeme_bind(ctx, ql, qm, cs, lex, result);

} /* name_bind */

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

    lextype_register(LEXTYPE_NAME, name_bind);

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

void
parser_insert_seq (parse_ctx_t pctx, lexseq_t *seq) {

    lexer_insert_seq(pctx->lexctx, seq);

} /* parser_insert_seq */

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
    lexseq_t result;
    lextype_t lt;
    int status;

    lexseq_init(&result);

    while (1) {
        lex = lexer_next(pctx->lexctx, pctx->no_eof);
        lt = lexeme_type(lex);
        if (lt == LEXTYPE_NONE || lt == LEXTYPE_END) {
            break;
        }
        lexeme_getpos(lex, &pctx->fileno, &pctx->lineno, &pctx->colno);
        status = lexeme_bind(pctx, pctx->quotelevel, pctx->quotemodifier,
                             pctx->condstate[pctx->condlevel], lex, &result);
        if (status < 0) {
            /* XXX error condition */
            return LEXTYPE_NONE;
        }
        if (status > 0) {
            lexer_insert_seq(pctx->lexctx, &result);
            pctx->quotemodifier = QM_NONE;
            continue;
        }
        lt = lexeme_type(lex);
        if (is_lexfunc(lt, 0)) {
            if (pctx->quotemodifier == QM_EXPAND ||
                pctx->quotelevel < QL_MACRO ||
                (lt == LEXTYPE_LXF_QUOTE || lt == LEXTYPE_LXF_UNQUOTE ||
                 lt == LEXTYPE_LXF_EXPAND)) {
                pctx->quotemodifier = QM_NONE;
                if (lexfuncs[lt-LEXTYPE_LXF_MIN] != 0) {
                    status = lexfuncs[lt-LEXTYPE_LXF_MIN](pctx, lt);
                    if (status < 0) {
                        /* XXX error condition */
                    } else if (status > 0) {
                        lexeme_free(lex);
                        continue;
                    } else {
                        break;
                    }
                }
            }
        }
        pctx->quotemodifier = QM_NONE;
        break;
    }
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

scopectx_t
parser_scope_get (parse_ctx_t pctx)
{
    return pctx->curscope;
    
} /* parser_scope_get */

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
 * parse_lexeme_seq
 *
 * Parses an arbitrary sequence of lexemes until hitting
 * a specified delimiter, returning the sequence as a chain.
 * Handles parentheses, brackets, etc.
 *
 * Can take the input lexemes from a chain (seq), or from the
 * parser.
 */
int
parse_lexeme_seq (parse_ctx_t pctx, lexseq_t *seq, lextype_t terms[],
                  int nterms, lexseq_t *result, lextype_t *term)
{
    lexeme_t *lex;
    lextype_t lt;
    int i;
    int depth[3], status, hit_term, private_seq;

    lexseq_init(result);
    depth[0] = depth[1] = depth[2] = 0;
    status = 1;
    hit_term = 0;
    private_seq = (seq != 0);

    while (status) {
        if (private_seq) {
            if (lexseq_length(seq) == 0) {
                if (term != 0) *term = LEXTYPE_END;
                break;
            }
            lex = lexseq_remhead(seq);
            lt = lexeme_boundtype(lex);
        } else {
            lt = parser_next(pctx, &lex);
        }
        if (lt == LEXTYPE_END) {
            // NB this would never happen with a private sequence
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
        lexseq_instail(result, lex);
    }

    return status;

} /* parse_lexeme_seq */

/*
 * --- end of public API ---
 *
 * All of the following routines are internal only and
 * implement lexical functions.
 *
 */

/*
 * %QUOTE
 *
 * Prevents the next lexeme from being bound (i.e., replaced
 * with its value, if it's a compile-time name, like a macro
 * or literal).  Only permitted at name-quote or macro-quote level.
 */
static int
parse_QUOTE (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt;

    if (pctx->quotelevel < QL_NAME) {
        /* XXX error condition */
        return 1;
    }

    lex = lexer_peek(pctx->lexctx, pctx->no_eof);
    lt = lexeme_boundtype(lex);
    // %QUOTE only applies to names, lexical function names,
    // commas, and percent signs.
    if (!is_name(lt) &&
        lt != LEXTYPE_DELIM_COMMA &&
        lt != LEXTYPE_LXF_DELIM_PERCENT) {
        /* XXX error condition */
        return 1;
    }

    if (lt == LEXTYPE_NAME) {
        name_t *np = name_search(pctx->curscope, lex->text.ptr,
                                 lex->text.len, 0);
        if (np != 0) {
            lt = name_type(np);
            // The lexical conditional functions are not %QUOTE-able
            if (lt >= LEXTYPE_LXF_IF && lt <= LEXTYPE_LXF_FI) {
                /* XXX error condition */
                return 1;
            }
        }
    }

    pctx->quotemodifier = QM_QUOTE;

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
parse_unquote_expand (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    name_t *np;
    lextype_t lt;

    if (pctx->quotelevel < ((curlt == LEXTYPE_LXF_EXPAND) ? QL_MACRO : QL_NAME)) {
        /* XXX error condition */
        return 1;
    }
    lex = lexer_peek(pctx->lexctx, pctx->no_eof);
    lt = lexeme_boundtype(lex);
    if (!is_name(lt) && !is_lexfunc(lt, 0)) {
        /* XXX error condition */
        return 1;
    }
    if (lt == LEXTYPE_NAME) {
        np = name_search(pctx->curscope, lex->text.ptr, lex->text.len, 0);
        if (np == 0) {
            /* XXX error condition */
            return 1;
        }
        lt = name_type(np);
    }
    if ((curlt == LEXTYPE_LXF_EXPAND) &&
        !(is_lexfunc(lt, 0) || lt == LEXTYPE_NAME_MACRO)) {
        /* XXX error condition */
        return 1;
    }
    pctx->quotemodifier = (curlt == LEXTYPE_LXF_EXPAND ? QM_EXPAND : QM_UNQUOTE);

    return 1;

} /* parse_unquote_expand */

/*
 * parse_string_literal
 *
 * %ASCII's', %ASCIZ's', %ASCIC's'
 *
 * String literals (plain, null-terminated, counted).
 *
 * Note that 's' can be a string expression (e.g., %STRING(...) or
 * another instance of %ASCI[IZ]).
 */
static int
parse_string_literal (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str = lexeme_text(lex);

    if (lt != LEXTYPE_STRING || (curlt == LEXTYPE_LXF_ASCIC && str->len > 255)) {
        /* XXX error condition */
        lexeme_free(lex);
    } else {
        lexeme_t *newlex;
        if (curlt == LEXTYPE_LXF_ASCIZ) {
            static strdesc_t nullchr = STRZDEF("");
            str = string_append(str, &nullchr);
        } else if (curlt == LEXTYPE_LXF_ASCIC) {
            lt = LEXTYPE_CSTRING;
        }
        newlex = parser_lexeme_create(pctx, lt, str);
        lexer_insert(pctx->lexctx, newlex);
        lexeme_free(lex);
    }
    return 1;
    
} /* parse_string_literal */

/*
 * parse_numeric_literal
 *
 * Common routine for parsing strings as numeric literals.
 *
 * Can begin with +/- sign, followed by one or more digits
 * in the specified 'base'.
 */
static int
parse_numeric_literal (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str;
    char buf[32], *cp;
    long numval;
    int base[] = { 2, 8, 10, 16 };

    if (lt != LEXTYPE_STRING) {
        /* XXX error condition */
        lexeme_free(lex);
        return 1;
    }
    str = lexeme_text(lex);
    cp = buf + (str->len >= sizeof(buf) ? sizeof(buf)-1 : str->len);
    // XXX probably need to do this ourselves, rather than rely on C lib
    memcpy(buf, str->ptr, cp-buf);
    *cp = '\0';
    errno = 0;
    numval = strtol(buf, &cp, base[curlt-LEXTYPE_LXF_B]);
    if (errno != 0 || (cp-buf) != str->len) {
        /* XXX error condition */
    } else {
        // XXX need to validate that the value fits into the
        // target machine's word length
        strdesc_t dsc;
        int len = snprintf(buf, sizeof(buf), "%ld", numval);
        strdesc_init(&dsc, buf, len);
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, &dsc));
    }
    string_free(str);
    lexeme_free(lex);

    return 1;
    
} /* parse_numeric_literal */

/*
 * %C'c'
 *
 * Numeric (ASCII) value of the character 'c'.
 *
 */
static int
parse_C (parse_ctx_t pctx, lextype_t curlt) {
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, &lex);
    strdesc_t *str = lexeme_text(lex);
    if (lt != LEXTYPE_STRING || str->len != 1) {
        /* XXX error condition */
    } else {
        char buf[8];
        int len = snprintf(buf, sizeof(buf), "%ld", (long)(*str->ptr & 0x7f));
        strdesc_t dsc;
        strdesc_init(&dsc, buf, len);
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, &dsc));
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
        lexeme_free(lex);
        if (lt != LEXTYPE_DELIM_LPAR) {
            /* XXX error condition */
            return &errlex;
        }
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
        lt = lexeme_boundtype(lex);
        switch (lt) {
            case LEXTYPE_NUMERIC:
            case LEXTYPE_CSTRING:
            case LEXTYPE_STRING:
                result = string_append(result, lexeme_text(lex));
                break;
            default:
                if (is_name(lt)) {
                    result = string_append(result, lexeme_text(lex));
                    break;
                }
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
    lex = parser_lexeme_create(pctx, LEXTYPE_STRING, result);
    string_free(result);
    return lex;

} /* string_params */

/*
 * %STRING(#p...)
 *
 * Forms a single string by concatenating the parameters.
 */
static int
parse_STRING (parse_ctx_t pctx, lextype_t curlt)
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
parse_EXACTSTRING (parse_ctx_t pctx, lextype_t curlt)
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
    if (len > STRING_MAXLEN) {
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
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_STRING, &nullstr));
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
    str = string_copy(0, lexeme_text(lex));
    if (str->len < len) {
        strdesc_t *filldsc;
        filldsc = string_alloc(0, len - str->len);
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
    lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_STRING, str));
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
parse_CHARCOUNT (parse_ctx_t pctx, lextype_t curlt)
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
    str = lexeme_text(lex);
    len = snprintf(buf, sizeof(buf), "%d", str->len);
    strdesc_init(&dsc, buf, len);
    lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, &dsc));
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
parse_CHAR (parse_ctx_t pctx, lextype_t curlt)
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
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_STRING, result));
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
parse_EXPLODE (parse_ctx_t pctx, lextype_t curlt)
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

    str = lexeme_text(lex);
    if (str->len == 0) {
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_STRING, str));
        string_free(str);
        lexeme_free(lex);
        return 1;
    }
    INITSTR(dsc, str->ptr, 1);
    for (remain = str->len; remain > 0; dsc.ptr += 1, remain -= 1) {
        lexeme_t *clex = parser_lexeme_create(pctx, LEXTYPE_STRING, &dsc);
        if (result == 0) {
            result = last = clex;
        } else {
            last->next = clex;
            last = clex;
        }
        if (remain > 1) {
            lexeme_t *clex = parser_lexeme_create(pctx, LEXTYPE_DELIM_COMMA, &comma);
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
 * %REMOVE(#p)
 * 
 * Removes a matching pair of parentheses, square brackets, or
 * angle brackets from the parameter, if there is such a pair.
 * Otherwise, the parameter remains unchanged.
 */
static int
parse_REMOVE (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lexseq_t result;
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

    if (!parse_lexeme_seq(pctx, 0, rpar, 1, &result, 0)) {
        /* XXX error condition */
        lexseq_free(&result);
        if (return_to_normal) {
            pctx->quotelevel = QL_NORMAL;
        }
        return 1;
    }

    if (return_to_normal) {
        pctx->quotelevel = QL_NORMAL;
    }

    if (lexseq_length(&result) == 0) {
        return 1;
    }

    // If the chain begins with an opener and ends with
    // its corresponding closer, trim them off before
    // inserting the chain back into the stream.  However,
    // we can't just blindly trim without checking to make
    // sure that they're really enclosing the entire sequence.
    // Otherwise, we'd trim '(A)+(B)', for instance.
    for (i = 0; i < 3; i++) {
        if (lexeme_boundtype(lexseq_head(&result)) == opener[i]) {
            break;
        }
    }
    if (i < 3 && lexseq_length(&result) > 1 &&
        lexeme_boundtype(lexseq_tail(&result)) == closer[i]) {
        int doit = 1;
        depth[0] = depth[1] = depth[2] = 0;
        for (lex = lexeme_next(lexseq_head(&result));
             lexeme_next(lex) != lexseq_tail(&result); lex = lexeme_next(lex)) {
            for (i = 0; doit && (i < 3); i++) {
                if (lexeme_boundtype(lex) == opener[i]) {
                    depth[i] += 1;
                } else if (lexeme_boundtype(lex) == closer[i]) {
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
            lex = lexseq_remhead(&result);
            lexeme_free(lex);
            lex = lexseq_remtail(&result);
            lexeme_free(lex);
        }
    }

    lexer_insert_seq(pctx->lexctx, &result);

    return 1;

} /* parse_REMOVE */

/*
 * parse_name_qname
 *
 * Common code for %NAME and %QUOTENAME.
 *
 * %NAME(#p,...)
 * %QUOTENAME(#p,...)
 *
 * Create a name from an aribtrary string.
 */
static int
parse_name_qname (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    lexseq_t resseq;
    lexeme_t *result;
    int status;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    result = parser_lexeme_create(pctx, LEXTYPE_NAME, lexeme_text(lex));
    lexseq_init(&resseq);
    status = lexeme_bind(pctx, pctx->quotelevel,
                         (curlt == LEXTYPE_LXF_QUOTENAME ? QM_QUOTE : QM_NONE),
                         pctx->condstate[pctx->condlevel], result, &resseq);
    if (status < 0) {
        /* XXX error condition */
        lexeme_free(result);
        return status;
    } else if (status > 0) {
        lexeme_free(result);
        lexer_insert_seq(pctx->lexctx, &resseq);
        return 1;
    }
    lexer_insert(pctx->lexctx, result);
    lexeme_free(lex);

    return 1;
    
} /* parse_name_qname */

/*
 * %NULL(#p,..)
 *
 * Returns 1 if all of the parameters are null, 0 otherwise.
 */
static int
parse_NULL (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lexseq_t seq;
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
        if (!parse_lexeme_seq(pctx, 0, terms, 2, &seq, &which)) {
            /* XXX error condition */
            if (return_to_normal) {
                pctx->quotelevel = QL_NORMAL;
            }
            return 1;
        }
        if (lexseq_length(&seq) != 0) {
            lexseq_free(&seq);
            allnull = 0;
        }
        if (which == LEXTYPE_DELIM_RPAR) {
            break;
        }
    }

    lexer_insert(pctx->lexctx,
                 parser_lexeme_create(pctx, LEXTYPE_NUMERIC, (allnull ? &one : &zero)));

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
parse_IDENTICAL (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt;
    int return_to_normal = pctx->quotelevel == QL_NORMAL;
    lexseq_t chain[2];
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

    if (parse_lexeme_seq(pctx, 0, &terms[0], 1, &chain[0], 0) &&
        parse_lexeme_seq(pctx, 0, &terms[1], 1, &chain[1], 0)) {
        lexer_insert(pctx->lexctx,
                     parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                        (lexemes_match(&chain[0], &chain[1]) ? &one : &zero)));
    } else {
        /* XXX error condition */
    }

    lexseq_free(&chain[0]);
    lexseq_free(&chain[1]);

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
parse_ISSTRING (parse_ctx_t pctx, lextype_t curlt)
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
        lexer_insert(pctx->lexctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
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
parse_IF (parse_ctx_t pctx, lextype_t curlt)
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
    if (lt != LEXTYPE_LXF_THEN) {
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
parse_ELSE (parse_ctx_t pctx, lextype_t curlt)
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
parse_FI (parse_ctx_t pctx, lextype_t curlt)
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
parse_REQUIRE (parse_ctx_t pctx, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    strdesc_t *str;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        /* XXX - error condition */
        lexeme_free(lex);
        return 1;
    }
    str = lexeme_text(lex);
    if (!lexer_newfile(pctx->lexctx, str->ptr, str->len)) {
        /* XXX - error condition */
    }
    lexeme_free(lex);
    string_free(str);
    return 1;

} /* parse_REQUIRE */

/*
 * %ASSIGN(#name, n)
 *
 * Assign a value to a compiletime constant.
 */
static int
parse_ASSIGN (parse_ctx_t pctx, lextype_t curlt)
{
    quotelevel_t oldql;
    lextype_t lt;
    name_t *np;
    lexeme_t *lex;

    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_LPAR) {
        /* XXX error condition */
        lexeme_free(lex);
        return 1;
    }
    oldql = pctx->quotelevel;
    pctx->quotelevel = QL_NAME;
    lt = parser_next(pctx, &lex);
    if (lexeme_boundtype(lex) != LEXTYPE_NAME_COMPILETIME) {
        /* XXX error condition */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    np = lexeme_ctx_get(lex);
    lexeme_free(lex);
    pctx->quotelevel = oldql;
    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_COMMA) {
        /* XXX error condition */
        lexeme_free(lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!parse_Expression(pctx)) {
        /* XXX error condition */
    } else {
        lt = parser_next(pctx, &lex);
        if (lt != LEXTYPE_NUMERIC) {
            /* XXX error condition */
        } else {
            *(long *)name_data(np) = lexeme_signedval(lex);
        }
        lexeme_free(lex);
    }
    lt = parser_next(pctx, &lex);
    if (lt != LEXTYPE_DELIM_RPAR) {
        /* XXX error condition */
    }
    lexeme_free(lex);
    return 1;
}
