/*
 *++
 *	File:			parser.c
 *
 *	Abstract:		Parsing and lexical functions
 *
 *  Module description:
 *		This module is the center of all lexical processing, containing
 * 		the main parsing functions and the implementations of all lexical
 *		functions. This module sits above the lexer module, which manages
 *		the lexeme stream.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		21-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
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
#include "declarations.h"
#include "macros.h"
#include "symbols.h"
#include "logging.h"
#include "machinedef.h"
#include "utils.h"

// Parser context structure
struct parse_ctx_s {
    namectx_t       namectx;
    scopectx_t      kwdscope, curscope;
    void            *expctx;
    machinedef_t    *mach;
    lexer_ctx_t     lexctx;
    int             lib_compile;
    quotemodifier_t quotemodifier;
    condstate_t     condstate[64];
    int             condlevel;
    int             no_eof;
    textpos_t       curpos;
    unsigned long   valmask;
    punctclass_t    punctclass;
    lextype_t       separator;
    lexctx_t        lmemctx;
    logctx_t        logctx;
};


// Lexical functions are implemented through a dispatch table keyed
// off the lexeme type code.  The type code is passed to the dispatched
// routine, so one routine can implement multiple variants of the same
// basic function.

typedef int (*lexfunc_t)(parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt);

// Factory macros
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
    DODEF(ASSIGN, parse_ASSIGN) DODEF(DECLARED, parse_DECLARED) \
    DODEF(NUMBER, parse_NUMBER) DODEF(NBITS, parse_nbits_func) \
    DODEF(NBITSU, parse_nbits_func) DODEF(PRINT, parse_msgfunc) \
    DODEF(CTCE, parse_xTCE) DODEF(LTCE, parse_xTCE) \
    DODEF(FIELDEXPAND, parse_FIELDEXPAND) \
    DODEF(ALLOCATION, parse_ALLOCATION) DODEF(SIZE, parse_SIZE) \
    DODEF(INFORM, parse_msgfunc) DODEF(ERROR, parse_msgfunc) \
    DODEF(WARN, parse_msgfunc) DODEF(MESSAGE, parse_msgfunc)

// Forward declarations for the routines; fortunately, multiple forward
// declarations aren't a problem.
#define DODEF(name_, rtn_) static int rtn_ (parse_ctx_t, quotelevel_t, lextype_t);
DODEFS
#undef DODEF

// The dispatch table.
#define DODEF(name_, rtn_) [LEXTYPE_LXF_##name_-LEXTYPE_LXF_MIN] = rtn_,
static lexfunc_t lexfuncs[LEXTYPE_LXF_MAX-LEXTYPE_LXF_MIN+1] = {
    DODEFS
};
#undef DODEF

// The name table definitions for the lexical functions.
#define DODEF(name_, rtn_) NAMEDEF("%" #name_, LEXTYPE_LXF_##name_, NAME_M_RESERVED),
static namedef_t parser_names[] = {
    DODEFS
    NAMEDEF("%THEN", LEXTYPE_LXF_THEN, NAME_M_RESERVED),
};
#undef DODEF

// Some frequently-used static definitions
static lexeme_t errlex = { 0, LEXTYPE_NONE };

static lextype_t opener[3] = {
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_LBRACK };
static lextype_t closer[3] = {
    LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RANGLE, LEXTYPE_DELIM_RBRACK };

static strdesc_t nullstr = STRDEF(""), one = STRDEF("1"), zero = STRDEF("0");

// Utility functions for identifying lexeme classes
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
static int is_attr(lextype_t lt) {
    return (lt >= LEXTYPE_ATTR_MIN && lt <= LEXTYPE_ATTR_MAX);
}

/*
 * parser_lexeme_add
 *
 * Internal utility function that creates a lexeme from some text and adds
 * it to the front of the lexeme stream.
 */
static void
parser_lexeme_add (parse_ctx_t pctx, lextype_t lt, strdesc_t *text)
{
    lexeme_t *lex = parser_lexeme_create(pctx, lt, text);
    lexer_insert(pctx->lexctx, lex);

} /* parser_lexeme_add */

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
name_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
              lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    parse_ctx_t pctx = ctx;
    strdesc_t *ltext = lexeme_text(lex);
    name_t *np;
    lextype_t nt;

    np = name_search(pctx->curscope, ltext->ptr, ltext->len, &nt);

    // Check for lexical conditional skips
    if (cs == COND_CWA) {
        if (np == 0 || (nt != LEXTYPE_LXF_ELSE && nt != LEXTYPE_LXF_FI)) {
            lexeme_free(lctx, lex);
            return 1;
        }
    } else if (cs == COND_AWC) {
        if (np == 0 || nt != LEXTYPE_LXF_FI) {
            lexeme_free(lctx, lex);
            return 1;
        }
    }

    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    if (ql == QL_MACRO) {
        if (np == 0 || (is_name(nt) && nt != LEXTYPE_NAME_MAC_PARAM)) {
            lex->type = LEXTYPE_UNBOUND;
            lex->boundtype = LEXTYPE_NAME;
            return 0;
        }
    } else if (ql == QL_NAME) {
        if (np == 0 || (is_name(nt) && nt != LEXTYPE_NAME_MACRO)) {
            lex->type = LEXTYPE_UNBOUND;
            lex->boundtype = LEXTYPE_NAME;
            return 0;
        }
    }

    lexeme_ctx_set(lex, np);
    if (np == 0) {
        lex->boundtype = lex->type = LEXTYPE_NAME;
        return 0;
    }

    lex->boundtype = nt;

    // Now that we've resolved the name, repeat the
    // binding process on the resulting lexeme.

    return lexeme_bind(lctx, ctx, ql, qm, cs, lex, result);

} /* name_bind */

/*
 * name_XXX_bind
 *
 * Default binding routine for LEXTYPE_NAME_xxx lextypes.  This can
 * be overridden if special handling is required.
 */
static int
name_XXX_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lctx, lex);
        return 1;
    }

    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        lex->boundtype = LEXTYPE_NAME;
        return 0;
    }

    if (ql == QL_MACRO && lt != LEXTYPE_NAME_MAC_PARAM) {
        lex->type = LEXTYPE_UNBOUND;
        lex->boundtype = LEXTYPE_NAME;
        return 0;
    }

    if (ql == QL_NAME && lt != LEXTYPE_NAME_MACRO) {
        lex->type = LEXTYPE_UNBOUND;
        lex->boundtype = LEXTYPE_NAME;
        return 0;
    }

    lex->type = lt;

    return 0;

} /* name_XXX_bind */

/*
 * select_punctclass
 *
 * This function sets the current punctuation class
 * for iterative macro expansion, based on the lexeme
 * that was just processed.  This information is used
 * by the macros module when expanding iterative macros.
 */
static void
select_punctclass (parse_ctx_t pctx, lextype_t lt)
{
    punctclass_t which;
    lextype_t separator;

    separator = LEXTYPE_NONE;

    if (is_operator(lt)) {
        which = PUNCT_OPERSEP_NOGROUP;
        separator = lt;
    } else if (is_name(lt) || is_attr(lt)) {
        which = PUNCT_COMMASEP_PARENS;
        separator = LEXTYPE_DELIM_COMMA;
    } else switch (lt) {
        case LEXTYPE_DELIM_LPAR:
        case LEXTYPE_DELIM_LBRACK:
        case LEXTYPE_DELIM_LANGLE:
        case LEXTYPE_DELIM_COMMA:
            which = PUNCT_COMMASEP_NOGROUP;
            separator = LEXTYPE_DELIM_COMMA;
            break;
        case LEXTYPE_EXP_DELIM_BEGIN:
        case LEXTYPE_DELIM_SEMI:
        case LEXTYPE_KWD_SET:
            which = PUNCT_SEMISEP_NOGROUP;
            separator = LEXTYPE_DELIM_SEMI;
            break;
        case LEXTYPE_DELIM_RPAR:
        case LEXTYPE_DELIM_RBRACK:
        case LEXTYPE_DELIM_RANGLE:
        case LEXTYPE_EXP_DELIM_END:
        case LEXTYPE_NUMERIC:
        case LEXTYPE_STRING:
        case LEXTYPE_CSTRING:
        case LEXTYPE_KWD_TES:
            which = PUNCT_COMMASEP_PARENS;
            separator = LEXTYPE_DELIM_COMMA;
            break;
        default:
            which = PUNCT_COMMASEP_NOGROUP; // whatever
            break;
    }

    pctx->punctclass = which;
    pctx->separator = separator;

} /* select_punctclass */

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
parser_init (namectx_t namectx, machinedef_t *mach, scopectx_t *kwdscopep,
             logctx_t logctx)
{
    parse_ctx_t pctx;
    scopectx_t kwdscope;
    lextype_t lt;
    int i;

    if (namectx == 0) {
        namectx = nametables_init(logctx);
    }
    kwdscope = scope_begin(namectx, 0);
    for (i = 0; i < sizeof(parser_names)/sizeof(parser_names[0]); i++) {
        name_declare(kwdscope, &parser_names[i], 0, 0, 0, 0);
    }

    pctx = malloc(sizeof(struct parse_ctx_s));
    if (pctx != 0) {
        memset(pctx, 0, sizeof(struct parse_ctx_s));
        pctx->logctx   = logctx;
        pctx->namectx  = namectx;
        pctx->kwdscope = kwdscope;
        if (kwdscopep != 0) {
            *kwdscopep = kwdscope;
        }
        pctx->curscope = scope_begin(namectx, kwdscope);
        pctx->lexctx = lexer_init(pctx->kwdscope, logctx);
        if (pctx->lexctx != 0) {
            pctx->lmemctx = lexer_lexctx(pctx->lexctx);
        }
        pctx->separator = LEXTYPE_NONE;
        pctx->punctclass = PUNCT_SEMISEP_NOGROUP;
    }

    // Register the default binding routine for names
    for (lt = LEXTYPE_NAME_MIN; lt <= LEXTYPE_NAME_MAX; lt++) {
        lextype_register(pctx->lmemctx, lt, name_XXX_bind);
    }
    lextype_register(pctx->lmemctx, LEXTYPE_NAME, name_bind);

    pctx->mach = mach;
    if (machine_scalar_bits(mach) == sizeof(pctx->valmask)*8) {
        pctx->valmask = -1;
    } else {
        pctx->valmask = (1UL << machine_scalar_bits(mach)) - 1;
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
 * Parser context setter/getters
 */
textpos_t parser_curpos(parse_ctx_t pctx) { return pctx->curpos; }
void *parser_get_expctx (parse_ctx_t pctx) { return pctx->expctx; }
void parser_set_expctx (parse_ctx_t pctx, void *expctx) { pctx->expctx = expctx;}
lexctx_t parser_lexmemctx (parse_ctx_t pctx) { return pctx->lmemctx; }
logctx_t parser_logctx (parse_ctx_t pctx) { return pctx->logctx; }
machinedef_t *parser_get_machinedef (parse_ctx_t pctx) { return pctx->mach; }
void parser_punctclass_set (parse_ctx_t pctx, punctclass_t cl, lextype_t sep) {
    pctx->punctclass = cl; pctx->separator = sep; }
void parser_punctclass_get (parse_ctx_t pctx, punctclass_t *clp, lextype_t *sepp) {
    *clp = pctx->punctclass; *sepp = pctx->separator; }
void parser_incr_erroneof (parse_ctx_t pctx) { pctx->no_eof += 1; }
void parser_decr_erroneof (parse_ctx_t pctx) { pctx->no_eof -= 1; }


/*
 * parser_fopen
 *
 * Begin parsing a file.
 */
int
parser_fopen (parse_ctx_t pctx, const char *fname, size_t fnlen,
              const char *suffix)
{
    return lexer_fopen(pctx->lexctx, fname, fnlen, suffix);

} /* parser_fopen */

/*
 * parser_popen
 *
 * Begin parsing from a programmed source.
 */
int
parser_popen (parse_ctx_t pctx, scan_input_fn inpfn, void *fnctx)
{
    return lexer_popen(pctx->lexctx, inpfn, fnctx);
}

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
 * parser_insert_seq
 *
 * Inserts a sequence of lexemes at the front of the
 * parsing stream.  The sequence is consumed by this
 * call.
 */
void
parser_insert_seq (parse_ctx_t pctx, lexseq_t *seq) {

    textpos_t pos = pctx->curpos;
    lexer_insert_seq_with_pos(pctx->lexctx, seq, pos);

} /* parser_insert_seq */

/*
 * parser_lexeme_create
 *
 * Utility function that wraps lexeme_create and inserts the current
 * text position in the created lexeme.
 */
lexeme_t *
parser_lexeme_create (parse_ctx_t pctx, lextype_t lt, strdesc_t *text) {
    lexeme_t *lex = lexeme_create(pctx->lmemctx, lt, text);
    lexeme_textpos_set(lex, pctx->curpos);
    return lex;

} /* parser_lexeme_create */

/*
 * parser_punct_grouper
 *
 * Returns the appropriate "grouper" lexeme based on the
 * current punctuation class.  If 'docloser' is 1, the
 * closing grouper is returned instead of the opening grouper.
 */
lexeme_t *
parser_punct_grouper (parse_ctx_t pctx, int docloser)
{
    lexeme_t *lex = 0;
    static strdesc_t lparen = STRDEF("("), rparen = STRDEF(")");
    static strdesc_t kwdset = STRDEF("SET"), kwdtes = STRDEF("TES");

    switch (pctx->punctclass) {
        case PUNCT_COMMASEP_NOGROUP:
        case PUNCT_OPERSEP_NOGROUP:
        case PUNCT_SEMISEP_NOGROUP:
            break;
        case PUNCT_COMMASEP_PARENS:
            if (docloser) {
                lex = parser_lexeme_create(pctx, LEXTYPE_DELIM_RPAR, &rparen);
            } else {
                lex = parser_lexeme_create(pctx, LEXTYPE_DELIM_LPAR, &lparen);
            }
            break;
        case PUNCT_SEMISEP_SETTES:
            if (docloser) {
                lex = parser_lexeme_create(pctx, LEXTYPE_KWD_TES, &kwdtes);
            } else {
                lex = parser_lexeme_create(pctx, LEXTYPE_KWD_SET, &kwdset);
            }
            break;
    }

    return lex;

} /* parser_punct_grouper */

/*
 * parser_punct_separator
 *
 * Returns a lexeme for the separator based on the current
 * punctuation class.
 */
lexeme_t *
parser_punct_separator (parse_ctx_t pctx)
{
    lexeme_t *lex = 0;
    static strdesc_t operstrs[] = {
        STRDEF("+"), STRDEF("-"), STRDEF("*"), STRDEF("/"), STRDEF("MOD"),
        STRDEF("="), STRDEF("."), STRDEF("AND"), STRDEF("EQV"), STRDEF("OR"),
        STRDEF("NOT"), STRDEF("XOR"), STRDEF("EQL"), STRDEF("NEQ"),
        STRDEF("LSS"), STRDEF("LEQ"), STRDEF("GTR"), STRDEF("GEQ"),
        STRDEF("EQLU"), STRDEF("NEQU"), STRDEF("LSSU"), STRDEF("LEQU"),
        STRDEF("GTRU"), STRDEF("GEQU"), STRDEF("EQLA"), STRDEF("NEQA"),
        STRDEF("LSSA"), STRDEF("LEQA"), STRDEF("GTRA"), STRDEF("GEQA")
    };
    static strdesc_t comma = STRDEF(","), semi = STRDEF(";");

    switch (pctx->punctclass) {
        case PUNCT_COMMASEP_NOGROUP:
        case PUNCT_COMMASEP_PARENS:
            lex = lexeme_create(pctx->lmemctx, LEXTYPE_DELIM_COMMA, &comma);
            break;
        case PUNCT_SEMISEP_NOGROUP:
        case PUNCT_SEMISEP_SETTES:
            lex = lexeme_create(pctx->lmemctx, LEXTYPE_DELIM_SEMI, &semi);
            break;
        case PUNCT_OPERSEP_NOGROUP:
            if (!is_operator(pctx->separator)) {
                log_signal(pctx->logctx, 0,
                           STC__INTCMPERR, "parser_punct_separator");
                return 0;
            }
            lex = parser_lexeme_create(pctx, pctx->separator,
            						   &operstrs[pctx->separator-LEXTYPE_OP_MIN]);
    }
    return lex;

} /* parser_punct_separator */


/*
 * parser_next
 *
 * Retrieve the next lexeme from the lexeme stream, applying whatever
 * lexical binding and processing that may be required based on the requested
 * quotelevel.
 *
 * The 'lexp' argument can be null, if the caller isn't interested in the
 * lexeme.  In that case this routine frees the lexeme, but still returns
 * the type code.
 *
 * If an error occurs, LEXTYPE_NONE is returned.  If we have hit the end
 * of the lexeme stream, LEXEME_END is returned.
 */
lextype_t
parser_next (parse_ctx_t pctx, quotelevel_t ql, lexeme_t **lexp)
{
    lexeme_t *lex;
    lexseq_t result;
    lextype_t lt;
    int status;

    lexseq_init(&result);

    while (1) {
    	// Get the next raw lexeme from the lexer
        lex = lexer_next(pctx->lexctx, pctx->no_eof);
        lt = lexeme_type(lex);
        if (lt == LEXTYPE_NONE || lt == LEXTYPE_END) {
            break;
        }
        pctx->curpos = lexeme_textpos_get(lex);
        // Bind the lexeme based on the current lexical context (quoting,
        // conditional state).  Returned status will be negative on error,
        // zero if processing should continue with the current lexeme
        // (it may have been modified), or positive if the 'result' sequence
        // should be used instead.
        status = lexeme_bind(pctx->lmemctx, pctx, ql, pctx->quotemodifier,
                             pctx->condstate[pctx->condlevel], lex, &result);
        if (status < 0) {
            log_signal(pctx->logctx, pctx->curpos, STC__INTCMPERR, "parser_next");
            return LEXTYPE_NONE;
        }
        if (status > 0) {
            lexer_insert_seq_with_pos(pctx->lexctx, &result, pctx->curpos);
            pctx->quotemodifier = QM_NONE;
            continue;
        }
        lt = lexeme_type(lex);
        select_punctclass(pctx, lt);
        // Re-bind numeric lexemes, taking into account the target machine's
        // word size.  XXX look at moving this to a binding hook XXX
        if (lt == LEXTYPE_NUMERIC) {
            long sval;
            sval = lexeme_signedval(lex);
            if (sval < 0) {
                if ((pctx->valmask & (-sval)) != (-sval)) {
                    log_signal(pctx->logctx, pctx->curpos, STC__NUMLITTRC, sval);
                    lexeme_val_setsigned(lex, sval | ~pctx->valmask);
                    string_printf(&lex->text, "%ld", (sval | ~pctx->valmask));
                }
            } else if ((pctx->valmask & sval) != sval) {
                log_signal(pctx->logctx, pctx->curpos, STC__NUMLITTRC, sval);
                lexeme_val_setsigned(lex, sval & pctx->valmask);
                string_printf(&lex->text, "%ld", (sval & pctx->valmask));
            }
        }
        // Dispatch for lexical function processing
        if (is_lexfunc(lt, 0)) {
            if (pctx->quotemodifier == QM_EXPAND || ql < QL_MACRO ||
                (lt == LEXTYPE_LXF_QUOTE || lt == LEXTYPE_LXF_UNQUOTE ||
                 lt == LEXTYPE_LXF_EXPAND)) {
                pctx->quotemodifier = QM_NONE;
                if (lexfuncs[lt-LEXTYPE_LXF_MIN] != 0) {
                    status = lexfuncs[lt-LEXTYPE_LXF_MIN](pctx, ql, lt);
                    if (status < 0) {
                        log_signal(pctx->logctx, pctx->curpos,
                                   STC__INTCMPERR, "parser_next");
                    } else if (status > 0) {
                        lexeme_free(pctx->lmemctx, lex);
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

    if (lexp == 0) {
        lexeme_free(pctx->lmemctx, lex);
    } else {
        *lexp = lex;
    }

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
    lextype_t lt;

    for (lt = parser_next(pctx, QL_NORMAL, 0); lt != LEXTYPE_END &&
         lt != LEXTYPE_NONE && lt != delimtype;
         lt = parser_next(pctx, QL_NORMAL, 0)) {
    }

} /* parser_skip_to_delim */

/*
 * Name scope manipulation
 *
 * get: returns the current scope.
 * push: pushes a new scope onto the scope stack.
 * pop: pops the current scope off the scope stack, without freeing it.
 * begin: creates a new empty scope and pushes it on the stack.
 * end: pops the current scope off the stack and frees it.
 */
scopectx_t parser_scope_get (parse_ctx_t pctx) { return pctx->curscope; }
scopectx_t parser_scope_push (parse_ctx_t pctx, scopectx_t newscope) {
    scope_setparent(newscope, pctx->curscope);
    pctx->curscope = newscope; return newscope; }
scopectx_t parser_scope_pop (parse_ctx_t pctx) {
    pctx->curscope = scope_getparent(pctx->curscope); return pctx->curscope; }
scopectx_t parser_scope_begin (parse_ctx_t pctx) {
    pctx->curscope = scope_begin(pctx->namectx, pctx->curscope);
    return pctx->curscope; }
scopectx_t parser_scope_end (parse_ctx_t pctx) {
    pctx->curscope = scope_end(pctx->curscope); return pctx->curscope; }

/*
 * parser_expect
 *
 * Convenience routine for parsing an expected lexeme type.
 * Lexeme can be returned, if desired.  If 'putbackonerror' is
 * set, the lexeme is put back if it didn't match the expected type.
 *
 * Returns: 1=ok, 0=err
 */
int
parser_expect (parse_ctx_t pctx, quotelevel_t ql, lextype_t expected_lt,
               lexeme_t **lexp, int putbackonerr)
{
    if (parser_expect_oneof(pctx, ql, &expected_lt, 1, lexp, putbackonerr) < 0) {
        return 0;
    }
    return 1;

} /* parser_expect */

/*
 * parser_expect_oneof
 *
 * Common routine for parsing one of a set of lexeme types.
 * The matching lexeme can be returned, if desired.  Callers can
 * use LEXTYPE_NONE to put "holes" in the array that won't match.
 *
 * If 'putbackonerror' is set, the lexeme is put back into the
 * stream if there was no match.
 *
 * Returns: array index on success, -1 on failure
 */
int
parser_expect_oneof (parse_ctx_t pctx, quotelevel_t ql, lextype_t expected_lts[],
                     int numlts, lexeme_t **lexp, int putbackonerr)
{
    lextype_t lt;
    lexeme_t *lex;
    int i;

    lt = parser_next(pctx, ql, &lex);
    for (i = 0; i < numlts; i++) {
        if (lt == expected_lts[i] && lt != LEXTYPE_NONE) {
            if (lexp != 0) {
                *lexp =  lex;
            } else {
                lexeme_free(pctx->lmemctx, lex);
            }
            return i;
        }
    }
    if (putbackonerr) {
        lexer_insert(pctx->lexctx, lex);
    } else {
        lexeme_free(pctx->lmemctx, lex);
    }
    return -1;

} /* parser_expect_oneof */

/*
 * parse_lexeme_seq
 *
 * Parses an arbitrary sequence of lexemes until hitting
 * one of a set of specified terminators, returning the
 * sequence that was parsed.
 *
 * Handles parentheses, brackets, etc., noting the depth
 * of bracketing and returning an error status if the sequence
 * terminates with an unclosed pair of brackets, or if a
 * closing bracket occurs with no corresponding opener.
 *
 * Can take the input lexemes from a sequence provided by the
 * caller, or from the parser's normal input stream.
 *
 * The special MARKER lextype is used to track the end of
 * the caller's provided lexeme sequence.  In the event that
 * parsing terminates before the end of the sequence, any
 * remaining lexemes (and the marker) are removed from the
 * parser's input stream.
 *
 * Returns 1 if successful, 0 otherwise.
 */
int
parse_lexeme_seq (parse_ctx_t pctx, lexseq_t *seq, quotelevel_t ql,
                  lextype_t terms[], int nterms,
                  lexseq_t *result, lextype_t *term)
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

    if (private_seq) {
        if (lexseq_length(seq) == 0) {
            if (term) *term = LEXTYPE_MARKER;
            return 1;
        }
        parser_insert(pctx, lexeme_create(pctx->lmemctx, LEXTYPE_MARKER, &nullstr));
        parser_insert_seq(pctx, seq);
    }

    while (status) {
        lt = parser_next(pctx, ql, &lex);
        if (lt == LEXTYPE_MARKER) {
            if (term != 0) *term = lt;
            break;
        }
        if (lt == LEXTYPE_END) {
            // NB this *should* never happen with a private sequence
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
            lexeme_free(pctx->lmemctx, lex);
            break;
        }
        for (i = 0; i < 3; i++) {
            if (lt == opener[i]) {
                depth[i] += 1;
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

    // Return any remaining private sequence lexemes
    // to the caller
    if (private_seq && lt != LEXTYPE_MARKER) {
        while (parser_next(pctx, ql, &lex) != LEXTYPE_MARKER) {
            lexseq_instail(seq, lex);
        }
        lexeme_free(pctx->lmemctx, lex); // frees the marker
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
 * Prevents the next lexeme from being bound.
 * Only permitted at name-quote or macro-quote level.
 */
static int
parse_QUOTE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt;

    if (ql < QL_NAME) {
        log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
        return 1;
    }

    lex = lexer_peek(pctx->lexctx, pctx->no_eof);
    lt = lexeme_boundtype(lex);
    // %QUOTE only applies to names, lexical function names,
    // commas, and percent signs.
    if (!is_name(lt) &&
        lt != LEXTYPE_DELIM_COMMA &&
        lt != LEXTYPE_LXF_DELIM_PERCENT) {
        log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
        return 1;
    }

    if (lt == LEXTYPE_NAME) {
        name_t *np = name_search(pctx->curscope, lex->text.ptr,
                                 lex->text.len, &lt);
        if (np != 0) {
            // The lexical conditional functions are not %QUOTE-able
            if (lt >= LEXTYPE_LXF_IF && lt <= LEXTYPE_LXF_FI) {
                log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
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
parse_unquote_expand (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    name_t *np;
    lextype_t lt;

    if (ql < ((curlt == LEXTYPE_LXF_EXPAND) ? QL_MACRO : QL_NAME)) {
        log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
        return 1;
    }
    lex = lexer_peek(pctx->lexctx, pctx->no_eof);
    lt = lexeme_boundtype(lex);
    if (!is_name(lt) && !is_lexfunc(lt, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
        return 1;
    }
    if (lt == LEXTYPE_NAME) {
        np = name_search(pctx->curscope, lex->text.ptr, lex->text.len, &lt);
        if (np == 0) {
            log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
            return 1;
        }
    }
    if ((curlt == LEXTYPE_LXF_EXPAND) &&
        !(is_lexfunc(lt, 0) || lt == LEXTYPE_NAME_MACRO)) {
        log_signal(pctx->logctx, pctx->curpos, STC__QUOFNCERR);
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
parse_string_literal (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, ql, &lex);
    strdesc_t *str = lexeme_text(lex);

    if (lt != LEXTYPE_STRING || (curlt == LEXTYPE_LXF_ASCIC && str->len > 255)) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexeme_free(pctx->lmemctx, lex);
    } else {
        if (curlt == LEXTYPE_LXF_ASCIZ) {
            static strdesc_t nullchr = STRZDEF("");
            str = string_append(str, &nullchr);
        } else if (curlt == LEXTYPE_LXF_ASCIC) {
            lt = LEXTYPE_CSTRING;
        }
        parser_lexeme_add(pctx, lt, str);
        lexeme_free(pctx->lmemctx, lex);
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
 *
 * NB: Assumes that the lextype codes for %B, %O, %DECIMAL,
 *     and %X are together in the lextype_t enumeration, in
 *     that order.
 */
static int
parse_numeric_literal (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, QL_NORMAL, &lex);
    strdesc_t *str;
    long val;
    int base[] = { 2, 8, 10, 16 };

    if (lt != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexeme_free(pctx->lmemctx, lex);
        return 1;
    }
    str = lexeme_text(lex);
    if (!string_numval(str, base[curlt-LEXTYPE_LXF_B], &val)) {
        log_signal(pctx->logctx, pctx->curpos, STC__NUMCNVERR, str->ptr, str->len);
        lexeme_free(pctx->lmemctx, lex);
        return 1;
    }
    str = string_printf(0, "%ld", val);
    parser_lexeme_add(pctx, LEXTYPE_NUMERIC, str);
    string_free(str);
    lexeme_free(pctx->lmemctx, lex);

    return 1;

} /* parse_numeric_literal */

/*
 * %C'c'
 *
 * Numeric (ASCII) value of the character 'c'.
 *
 */
static int
parse_C (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt) {
    lexeme_t *lex;
    lextype_t lt = parser_next(pctx, ql, &lex);
    strdesc_t *str = lexeme_text(lex);
    if (lt != LEXTYPE_STRING || str->len != 1) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
    } else {
        char buf[8];
        int len = snprintf(buf, sizeof(buf), "%ld", (long)(*str->ptr & 0x7f));
        strdesc_t dsc;
        strdesc_init(&dsc, buf, len);
        parser_lexeme_add(pctx, LEXTYPE_NUMERIC, &dsc);
    }
    string_free(str);
    lexeme_free(pctx->lmemctx, lex);

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
    strdesc_t *result = string_alloc(0,0);

    if (!already_have_open_paren) {
        lt = parser_next(pctx, QL_NORMAL, &lex);
        lexeme_free(pctx->lmemctx, lex);
        if (lt != LEXTYPE_DELIM_LPAR) {
            log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
            return &errlex;
        }
    }

    while (1) {
        lt = parser_next(pctx, QL_NAME, &lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            lexeme_free(pctx->lmemctx, lex);
            break;
        }
        // It's OK to have a null argument
        if (lt == LEXTYPE_DELIM_COMMA) {
            lexeme_free(pctx->lmemctx, lex);
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
                lexeme_free(pctx->lmemctx, lex);
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
                return &errlex;
        }
        // OK, now we expect a comma or closing paren
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            lexeme_free(pctx->lmemctx, lex);
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ",");
            lexeme_free(pctx->lmemctx, lex);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return &errlex;
        }
        // otherwise, continue
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
parse_STRING (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
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
parse_EXACTSTRING (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt;
    char fillchr = 0;
    size_t len = 0;
    strdesc_t *str;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    if (!expr_parse_ctce(pctx->expctx, &lex)) {
        log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    len = lexeme_unsignedval(lex);
    lexeme_free(pctx->lmemctx, lex);
    if (len > STRING_MAXLEN) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 0)) {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!expr_parse_ctce(pctx->expctx, &lex)) {
        log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    fillchr = lexeme_unsignedval(lex) & 0xff;
    if (lexeme_signedval(lex) > 255) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVCHRVAL, lexeme_text(lex));
        lexeme_free(pctx->lmemctx, lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lexeme_free(pctx->lmemctx, lex);
    lt = parser_next(pctx, QL_NORMAL, &lex);
    lexeme_free(pctx->lmemctx, lex);
    if (lt == LEXTYPE_DELIM_RPAR) {
        parser_lexeme_add(pctx, LEXTYPE_STRING, &nullstr);
        return 1;
    } else if (lt != LEXTYPE_DELIM_COMMA) {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }

    lex = string_params(pctx, 1);
    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexeme_free(pctx->lmemctx, lex);
        return 1;
    }
    str = string_copy(0, lexeme_text(lex));
    if (str->len < len) {
        strdesc_t *filldsc;
        filldsc = string_alloc(0, len - str->len);
        if (filldsc == 0) {
            log_signal(pctx->logctx, pctx->curpos, STC__OUTOFMEM);
        } else {
            memset(filldsc->ptr, fillchr, filldsc->len);
            str = string_append(str, filldsc);
            if (str == 0) {
                log_signal(pctx->logctx, pctx->curpos, STC__OUTOFMEM);
            }
            string_free(filldsc);
        }
    } else if (str->len > len) {
        str->len = len;
    }

    lexeme_free(pctx->lmemctx, lex);
    parser_lexeme_add(pctx, LEXTYPE_STRING, str);
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
parse_CHARCOUNT (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    strdesc_t *str, dsc;
    char buf[8];
    int len;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    str = lexeme_text(lex);
    len = snprintf(buf, sizeof(buf), "%d", str->len);
    strdesc_init(&dsc, buf, len);
    parser_lexeme_add(pctx, LEXTYPE_NUMERIC, &dsc);
    lexeme_free(pctx->lmemctx, lex);
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
parse_CHAR (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lextype_t lt;
    int skip_to_paren = 0;
    int hit_error = 0;
    strdesc_t chdsc;
    strdesc_t *result;
    char ch;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }

    result = string_alloc(0, 0);
    strdesc_init(&chdsc, &ch, 1);
    while (1) {
        if (!expr_parse_ctce(pctx->expctx, &lex)) {
            log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
            hit_error = 1;
            skip_to_paren = 1;
            break;
        }
        val = lexeme_signedval(lex);
        if (val > 255) {
            strdesc_t *text = lexeme_text(lex);
            log_signal(pctx->logctx, pctx->curpos, STC__NUMCNVERR,
                       text->ptr, text->len);
            skip_to_paren = 1;
            hit_error = 1;
            lexeme_free(pctx->lmemctx, lex);
            break;
        }
        ch = val & 0xff;
        result = string_append(result, &chdsc);
        lexeme_free(pctx->lmemctx, lex);
        lt = parser_next(pctx, QL_NORMAL, &lex);
        lexeme_free(pctx->lmemctx, lex);
        if (lt == LEXTYPE_DELIM_RPAR) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ",");
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
        parser_lexeme_add(pctx, LEXTYPE_STRING, result);
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
parse_EXPLODE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    strdesc_t *str, dsc;
    size_t remain;
    lexseq_t result;
    static strdesc_t comma = STRDEF(",");

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }

    lexseq_init(&result);
    str = lexeme_text(lex);
    if (str->len == 0) {
        parser_lexeme_add(pctx, LEXTYPE_STRING, str);
        string_free(str);
        lexeme_free(pctx->lmemctx, lex);
        return 1;
    }
    strdesc_init(&dsc, str->ptr, 1);
    for (remain = str->len; remain > 0; dsc.ptr += 1, remain -= 1) {
        lexseq_instail(&result, parser_lexeme_create(pctx, LEXTYPE_STRING, &dsc));
        if (remain > 1) {
            lexseq_instail(&result,
                           parser_lexeme_create(pctx, LEXTYPE_DELIM_COMMA, &comma));
        }
    }

    parser_insert_seq(pctx, &result);

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
parse_REMOVE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    lexseq_t result;
    int i;
    int depth[3];
    lextype_t rpar[1] = {LEXTYPE_DELIM_RPAR};

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }

    if (!parse_lexeme_seq(pctx, 0, QL_NAME, rpar, 1, &result, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__SYNTAXERR);
        lexseq_free(pctx->lmemctx, &result);
        return 1;
    }

    if (lexseq_length(&result) == 0) {
        return 1;
    }

    // If the sequence begins with an opener and ends with
    // its corresponding closer, trim them off before
    // inserting the sequence back into the stream.  However,
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
            lexeme_free(pctx->lmemctx, lex);
            lex = lexseq_remtail(&result);
            lexeme_free(pctx->lmemctx, lex);
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
parse_name_qname (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    lexseq_t resseq;
    lexeme_t *result;
    int status;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexer_insert(pctx->lexctx, lex);
        return 1;
    }
    result = parser_lexeme_create(pctx, LEXTYPE_NAME, lexeme_text(lex));
    lexseq_init(&resseq);
    status = lexeme_bind(pctx->lmemctx, pctx, ql,
                         (curlt == LEXTYPE_LXF_QUOTENAME ? QM_QUOTE : QM_NONE),
                         pctx->condstate[pctx->condlevel], result, &resseq);
    if (status < 0) {
        strdesc_t *text = lexeme_text(lex);
        log_signal(pctx->logctx, pctx->curpos, STC__INVNAME, text);
        lexeme_free(pctx->lmemctx, result);
        return status;
    } else if (status > 0) {
        lexeme_free(pctx->lmemctx, result);
        lexer_insert_seq(pctx->lexctx, &resseq);
        return 1;
    }
    lexer_insert(pctx->lexctx, result);
    lexeme_free(pctx->lmemctx, lex);

    return 1;

} /* parse_name_qname */

/*
 * %NULL(#p,..)
 *
 * Returns 1 if all of the parameters are null, 0 otherwise.
 */
static int
parse_NULL (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexseq_t seq;
    int allnull = 1;
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };
    lextype_t which;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }

    while (1) {
        if (!parse_lexeme_seq(pctx, 0, QL_NAME, terms, 2, &seq, &which)) {
            log_signal(pctx->logctx, pctx->curpos, STC__SYNTAXERR);
            return 1;
        }
        if (lexseq_length(&seq) != 0) {
            lexseq_free(pctx->lmemctx, &seq);
            allnull = 0;
        }
        if (which == LEXTYPE_DELIM_RPAR) {
            break;
        }
    }

    parser_lexeme_add(pctx, LEXTYPE_NUMERIC, (allnull ? &one : &zero));

    return 1;

} /* parse_NULL */

/*
 * %IDENTICAL(#s1, #s2)
 *
 * Returns 1 if the two sequences of lexemes are
 * identical.
 */
static int
parse_IDENTICAL (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexseq_t chain[2];
    lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }

    if (parse_lexeme_seq(pctx, 0, QL_NAME, &terms[0], 1, &chain[0], 0) &&
        parse_lexeme_seq(pctx, 0, QL_NAME, &terms[1], 1, &chain[1], 0)) {
        parser_lexeme_add(pctx, LEXTYPE_NUMERIC,
                        (lexemes_match(&chain[0], &chain[1]) ? &one : &zero));
    } else {
        log_signal(pctx->logctx, pctx->curpos, STC__SYNTAXERR);
    }

    lexseq_free(pctx->lmemctx, &chain[0]);
    lexseq_free(pctx->lmemctx, &chain[1]);

    return 1;

} /* parse_IDENTICAL */

/*
 * %ISSTRING(exp,...)
 *
 * Returns 1 if every expression results in a quoted-string
 * lexeme, otherwise 0.  Note that this up-calls to the expression
 * module, since the parameters are expressions.
 * XXX Architecturally cleaner to expose the lexical function
 * dispatch interface and have the expression module hook this
 * function itself? XXX
 */
static int
parse_ISSTRING (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    int allstr, status;

    status = expr_parse_ISSTRING(pctx->expctx, &allstr);

    if (status) {
        parser_lexeme_add(pctx, LEXTYPE_NUMERIC, (allstr ? &one : &zero));
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
parse_IF (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    int testval;

    // Note the use of increment/decrement here -- this is to handle
    // nesting of instances in which hitting end-of-file is a no-no.
    pctx->no_eof += 1;

    if (!expr_parse_ctce(pctx->expctx, &lex)) {
        log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
        pctx->no_eof -= 1;
        return 1;
    }

    testval = lexeme_signedval(lex) & 1;
    lexeme_free(pctx->lmemctx, lex);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_LXF_THEN, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__KWDEXP, "%THEN");
        pctx->no_eof -= 1;
        return 1;
    }

    if (pctx->condstate[pctx->condlevel] != COND_NORMAL) {
        if (pctx->condlevel >= sizeof(pctx->condstate)/sizeof(pctx->condstate[0])) {
            log_signal(pctx->logctx, pctx->curpos, STC__INTCMPERR, "parse_IF");
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
parse_ELSE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    condstate_t curstate = pctx->condstate[pctx->condlevel];
    if (curstate != COND_CWA && curstate != COND_CWC) {
        log_signal(pctx->logctx, pctx->curpos, STC__UNEXPCOND);
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
parse_FI (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    condstate_t curstate = pctx->condstate[pctx->condlevel];
    if (curstate == COND_NORMAL) {
        log_signal(pctx->logctx, pctx->curpos, STC__UNEXPCOND);
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
parse_REQUIRE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    strdesc_t *str;

    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        log_signal(pctx->logctx, pctx->curpos, STC__INVSTRLIT);
        lexeme_free(pctx->lmemctx, lex);
        return 1;
    }
    str = lexeme_text(lex);
    if (!parser_fopen(pctx, str->ptr, str->len, ".req")) {
        log_signal(pctx->logctx, pctx->curpos, STC__REQFILERR, str);
    }
    lexeme_free(pctx->lmemctx, lex);
    return 1;

} /* parse_REQUIRE */

/*
 * %ASSIGN(#name, n)
 *
 * Assign a value to a compiletime constant.
 */
static int
parse_ASSIGN (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    name_t *np;
    lexeme_t *lex;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    parser_next(pctx, QL_NAME, &lex);
    if (lexeme_boundtype(lex) != LEXTYPE_NAME_COMPILETIME) {
        log_signal(pctx->logctx, pctx->curpos, STC__CTNAMEXP);
        lexeme_free(pctx->lmemctx, lex);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    np = lexeme_ctx_get(lex);
    lexeme_free(pctx->lmemctx, lex);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ",");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!expr_parse_ctce(pctx->expctx, &lex)) {
        log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
    } else {
        compiletime_assign(np, lexeme_signedval(lex));
        lexeme_free(pctx->lmemctx, lex);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ")");
    }

    return 1;

} /* parse_ASSIGN */

/*
 * %NUMBER(n)
 *
 * Return a numeric lexeme representing the value
 * of 'n', which can be a (numeric) string literal,
 * a numeric literal, or the name of a COMPILETIME
 * or LITERAL.
 */
static int
parse_NUMBER (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lextype_t lt;
    lexeme_t *lex, *rlex;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    rlex = 0;
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt == LEXTYPE_STRING) {
        if (!string_numval(lexeme_text(lex), 10, &val)) {
            strdesc_t *text = lexeme_text(lex);
            log_signal(pctx->logctx, pctx->curpos, STC__NUMCNVERR,
                       text->ptr, text->len);
            lexeme_free(pctx->lmemctx, lex);
            rlex = lexeme_create(pctx->lmemctx, LEXTYPE_STRING, &zero);
        } else {
            lexeme_val_setsigned(lex, val);
            rlex = lex;
        }
    } else {
        parser_insert(pctx, lex);
        if (!expr_parse_ctce(pctx->expctx, &rlex)) {
            rlex = lexeme_create(pctx->lmemctx, LEXTYPE_STRING, &zero);
            log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ")");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        if (rlex != 0) {
            lexeme_free(pctx->lmemctx, rlex);
        }
        return 1;
    }

    lexer_insert(pctx->lexctx, rlex);

    return 1;

} /* parse_NUMBER */

/*
 * %DECLARED(#name)
 *
 * Returns 1 if the specified name has been explicitly
 * declared, 0 otherwise.
 */
static int
parse_DECLARED (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lextype_t boundlt;
    lexeme_t *lex, *rlex = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    parser_next(pctx, QL_NAME, &lex);
    boundlt = lexeme_boundtype(lex);
    if (!is_name(boundlt)) {
        strdesc_t *text = lexeme_text(lex);
        log_signal(pctx->logctx, pctx->curpos, STC__INVNAME, text);
        lexer_insert(pctx->lexctx, lex);
    } else {
        strdesc_t *text = lexeme_text(lex);
        rlex = lexeme_create(pctx->lmemctx, LEXTYPE_NUMERIC,
                             (name_is_declared(parser_scope_get(pctx),
                                               text->ptr, text->len) ?
                              &one : &zero));
        lexeme_free(pctx->lmemctx, lex);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ")");
        lexeme_free(pctx->lmemctx, rlex);
        rlex = 0;
    }
    if (rlex == 0) {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
    } else {
        lexer_insert(pctx->lexctx, rlex);
    }
    return 1;

} /* parse_DECLARED */

/*
 * %NBITS(n,...)
 * %NBITSU(n,..)
 *
 * Returns the largest number of bits required to hold the
 * value of the (compile-time-constant) expressions.  In the U-form,
 * the expressions are treated as unsigned values.
 */
static int
parse_nbits_func (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lextype_t lt;
    lexeme_t *lex;
    strdesc_t *bcstr;
    long maxbits = 0, thesebits;
    machinedef_t *mach = pctx->mach;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    while (1) {
        if (!expr_parse_ctce(pctx->expctx, &lex)) {
            log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return 1;
        }
        if (curlt == LEXTYPE_LXF_NBITS) {
            long val = lexeme_signedval(lex);
            thesebits = bits_needed((unsigned long) labs(val));
            if (val < 0 && thesebits < machine_scalar_bits(mach)-1) {
                thesebits = machine_scalar_bits(mach)-1;
            }
        } else {
            thesebits = bits_needed(lexeme_unsignedval(lex));
        }
        if (thesebits > maxbits) {
            maxbits = thesebits;
        }
        lexeme_free(pctx->lmemctx, lex);
        lt = parser_next(pctx, QL_NORMAL, 0);
        if (lt == LEXTYPE_DELIM_RPAR) {
            break;
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ",");
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return 1;
        }
    }
    if (maxbits > machine_scalar_bits(mach) - (curlt == LEXTYPE_LXF_NBITS ? 1 : 0)) {
        log_signal(pctx->logctx, pctx->curpos, STC__NBITSERR,
                   maxbits, machine_scalar_bits(mach));
        maxbits = machine_scalar_bits(mach);
    }
    bcstr = string_printf(0, "%ld", maxbits);
    parser_lexeme_add(pctx, LEXTYPE_NUMERIC, bcstr);
    string_free(bcstr);
    return 1;

} /* parse_nbits_func */

/*
 * parse_msgfunc
 *
 * Common code for %PRINT and friends
 *
 * %PRINT(#p,...)   - print a message in the listing
 * %MESSAGE(#p,...) - print a message on the console
 * %ERROR(#p,...)   - print an error message, and treat as an error
 * %WARN(#p,...)    - print a warning message, and treat as a warning
 * %INFORM(#p,...)  - print an informational message
 */
static int
parse_msgfunc (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex = string_params(pctx, 0);
    lextype_t lt = lexeme_boundtype(lex);
    textpos_t pos = lexeme_textpos_get(lex);

    if (lt == LEXTYPE_STRING) {
        strdesc_t *text = lexeme_text(lex);
        switch (curlt) {
            case LEXTYPE_LXF_PRINT:
                log_print(pctx->logctx, pos, text);
                break;
            case LEXTYPE_LXF_MESSAGE:
                log_message(pctx->logctx, pos, text);
                break;
            case LEXTYPE_LXF_ERROR:
                log_message(pctx->logctx, pos, text);
                break;
            case LEXTYPE_LXF_INFORM:
                log_inform(pctx->logctx, pos, text);
                break;
            case LEXTYPE_LXF_WARN:
                log_warn(pctx->logctx, pos, text);
                break;
            default:
                log_signal(pctx->logctx, pos, STC__INTCMPERR, "parse_msgfunc[1]");
                break;
        }
    } else {
        log_signal(pctx->logctx, pos, STC__INTCMPERR, "parse_msgfunc[2]");
    }

    lexeme_free(pctx->lmemctx, lex);

    return 1;

} /* parse_msg_func */

/*
 * parse_xTCE
 *
 * %CTCE(exp,..)
 * %LTCE(exp,..)
 *
 * Return 1 if the expression(s) are CTCEs/LTCEs.
 */
static int
parse_xTCE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    int allok, status;

    status = expr_parse_xCTE(pctx->expctx, (curlt == LEXTYPE_LXF_LTCE), &allok);
    if (status) {
        parser_lexeme_add(pctx, LEXTYPE_NUMERIC, (allok ? &one : &zero));
    }

    return status;

} /* parse_xTCE */

/*
 * parse_FIELDEXPAND
 *
 * %FIELDEXPAND(field-name {,n})
 */
static int
parse_FIELDEXPAND (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    name_t *fnp;

    if (!(parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1))) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
        return 1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_FIELD, &lex, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__FLDNAMEXP);
        return 1;
    }
    fnp = lexeme_ctx_get(lex);
    if (fnp == 0) {
        log_signal(pctx->logctx, pctx->curpos, STC__INTCMPERR, "parse_FIELDEXPAND");
        return 1;
    }
    lexeme_free(pctx->lmemctx, lex);
    lex = 0;
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        if (!expr_parse_ctce(pctx->expctx, &lex)) {
            log_signal(pctx->logctx, pctx->curpos, STC__EXPCTCE);
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ")");
    }
    if (lex == 0) {
        lexseq_t tmpseq;
        lexseq_init(&tmpseq);
        lexseq_copy(pctx->lmemctx, &tmpseq, field_lexseq(fnp));
        parser_insert_seq(pctx, &tmpseq);
    } else {
        unsigned int which = (unsigned int) lexeme_unsignedval(lex);
        lexeme_t *rlex = field_extract(fnp, which);
        lexeme_free(pctx->lmemctx, lex);
        if (rlex == 0) {
            log_signal(pctx->logctx, pctx->curpos, STC__FLDEXPERR);
            parser_insert(pctx, lexeme_create(pctx->lmemctx, LEXTYPE_NUMERIC, &zero));
        } else {
            parser_insert(pctx, lexeme_copy(pctx->lmemctx, rlex));
        }

    }

    return 1;

} /* parse_FIELDEXPAND */

/*
 * parse_ALLOCATION
 *
 * %ALLOCATION(name)
 *   Returns a numeric literal representing the number of addressable
 *   units allocated to <name> (which must have allocated storage).
 */
static int
parse_ALLOCATION (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    lexeme_t *lex;
    unsigned int units;
    int status = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, "(");
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_DATA, &lex, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__SEGNAMEXP);
    } else {
        status = expr_get_allocation(pctx->expctx, lexeme_text(lex), &units);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        log_signal(pctx->logctx, pctx->curpos, STC__DELIMEXP, ")");
    }

    if (status) {
        parser_insert(pctx,
                  parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                                       string_printf(0, "%lu", units)));
    }
    return 1;

} /* parse_ALLOCATION */

/*
 * parse_SIZE
 *
 * %SIZE(structure-attribute)
 *
 * Returns the number of addressable units that would
 * be allocated for a data segment with the specified
 * structure attribute.
 */
static int
parse_SIZE (parse_ctx_t pctx, quotelevel_t ql, lextype_t curlt)
{
    unsigned int units;
    int status;

    status = expr_parse_SIZE(pctx->expctx, &units);
    if (status) {
        parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                                                 string_printf(0, "%u", units)));
    }
    return status;

} /* parse_SIZE */
