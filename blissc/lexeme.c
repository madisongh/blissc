/*
 *++
 *	File:			lexeme.c
 *
 *	Abstract:		Lexeme handling
 *
 *  Module description:
 *		This module implements the basic handling of lexemes.
 *		It provides memory management of the lexeme pool, routines
 *		for manipulating lexemes and lexeme sequences (along with the
 *		inlined functions in the counterpart header file), and
 *		the plug-in framework for lexical binding that the parser and other
 *		modules use.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		22-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "lexeme.h"
#include "strings.h"

#define DOLEXTYPE(lt) "LEXTYPE_" #lt,
static const char *ltnames[] = { DOLEXTYPES };
#undef DOLEXTYPE

#define ALLOC_QTY 512

struct lexctx_s {
    logctx_t         logctx;
    lexeme_t        *freepool;
    lextype_bind_fn  binders[LEXTYPE_COUNT];
};

static lexeme_t errlex = { 0, LEXTYPE_NONE };

/*
 * lextype_name
 *
 * Returns a string with the name of a lexeme type.
 * For debugging purposes only.
 */
const char *
lextype_name (lextype_t lt)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return "*LEXTYPE_OUTOFRANGE*";
    }
    return ltnames[lt];

} /* lextype_name */

/*
 * lextype_register
 *
 * Registers a lexical binding function.
 */
int
lextype_register (lexctx_t lctx, lextype_t lt, lextype_bind_fn binder)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return 0;
    }
    lctx->binders[lt] = binder;
    return 1;

} /* lextype_register */

/*
 * lexeme_alloc
 *
 * Internal allocation function.  Expands the lookaside list
 * when needed.
 */
static lexeme_t *
lexeme_alloc (lexctx_t lctx, lextype_t type, const char *text, size_t len)
{
    lexeme_t *lex;
    int i;

    if (lctx->freepool == 0) {
        lctx->freepool = malloc(ALLOC_QTY * sizeof(lexeme_t));
        if (lctx->freepool == 0) {
            log_signal(lctx->logctx, 0, STC__OUTOFMEM, "lexeme_alloc");
            return &errlex;
        }
        for (i = 0, lex = lctx->freepool; i < ALLOC_QTY-1; i++, lex++) {
            lex->tq_next = lex + 1;
        }
        lex->tq_next = 0;
    }

    lex = lctx->freepool;
    lctx->freepool = lex->tq_next;
    memset(lex, 0, sizeof(lexeme_t));
    string_alloc(&lex->text, len);
    if (text != 0) {
        string_from_chrs(&lex->text, text, len);
    }
    lex->type = lex->boundtype = type;
    lex->flags = LEX_M_ALLOCATED;
    return lex;
}

/*
 * lexeme_bind
 *
 * The core lexical binding function.  If a module has registered
 * its own binder for a given lexeme type, it is responsible for
 * all handling of current lexical state -- conditional, quoting,
 * etc.  For lexeme types that do not have a binder registered,
 * this routine performs the normal interpretation of lexical
 * state.  This means that if you implement anything for which
 * those normal rules may not apply (e.g., a lexical function or
 * macro-related lexeme type), you MUST register a binding function
 * to handle the exceptional cases.  DO NOT code any special-case
 * handling here.
 *
 * Returns:
 *  -1: error
 *   0: binding/unbinding occurred normally, original
 *      lexeme modified accordingly
 *   1: binding/unbinding resulted in modification of
 *      lexeme sequence (could be null result)
 */
int
lexeme_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
             condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    lextype_t lt = lexeme_boundtype(lex);

    if (lctx->binders[lt] != 0) {
        return lctx->binders[lt](lctx, ctx, ql, qm, lt, cs, lex, result);
    }

    // Check for lexical conditional skips
    if ((cs == COND_CWA && !(lt == LEXTYPE_LXF_ELSE || lt == LEXTYPE_LXF_FI)) ||
        (cs == COND_AWC && lt != LEXTYPE_LXF_FI)) {
        lexeme_free(lctx, lex);
        return 1;
    }

    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    if (lex->type != LEXTYPE_UNBOUND) {
        return 0;
    }

    if (lt == LEXTYPE_NUMERIC) {
        long val;
        strdesc_t *ltext = lexeme_text(lex);
        char *cp;

        errno = 0;
        val = strtol(ltext->ptr, &cp, 10);
        if (errno != 0) {
            char errbuf[64];
            errbuf[0] = '\0';
            strerror_r(errno, errbuf, sizeof(errbuf));
            log_signal(lctx->logctx, lexeme_textpos_get(lex),
                       STC__NUMCNVERR, errbuf, strlen(errbuf));
            return -1;
        }
        lexeme_val_setsigned(lex, val);
    } else if (lt == LEXTYPE_CSTRING) {
        strdesc_t *ltext = lexeme_text(lex);
        strdesc_t *cstr;
        lexeme_t *nlex;
        size_t len;

        len = ltext->len > 255 ? 255 : ltext->len;
        cstr = ascic_string_from_chrs(0, ltext->ptr, len);
        nlex = lexeme_alloc(lctx, lt, cstr->ptr, cstr->len);
        lexeme_copypos(nlex, lex);
        lexseq_instail(result, nlex);
        lexeme_free(lctx, lex);
        return 1;
    }

    lex->type = lt;
    return 0;

} /* lexeme_bind */

/*
 * lexeme_init
 *
 * Module initialization.  Called from the lexer module.
 */
lexctx_t
lexeme_init (logctx_t logctx)
{
    lexctx_t lctx = malloc(sizeof(struct lexctx_s));

    if (lctx != 0) {
        memset(lctx, 0, sizeof(struct lexctx_s));
        lctx->logctx = logctx;
    }
    return lctx;

} /* lexeme_init */

/*
 * lexeme_finish
 *
 * Shutdown routine.
 */
void
lexeme_finish (lexctx_t lctx)
{
    free(lctx);

} /* lexeme_finish */

/*
 * lexeme_create
 *
 * Public API for creating a lexeme from some value.
 */
lexeme_t *
lexeme_create (lexctx_t lctx, lextype_t type, strdesc_t *tok)
{
    lexeme_t *lex;

    if (type < LEXTYPE_MIN || type > LEXTYPE_MAX) {
        log_signal(lctx->logctx, 0, STC__INTCMPERR, "lexeme_create");
        return &errlex;
    }
    lex  = lexeme_alloc(lctx, LEXTYPE_UNBOUND, tok->ptr, tok->len);
    if (lex->type == LEXTYPE_NONE) {
        return &errlex;
    }
    lex->boundtype = type;

    return lex;

} /* lexeme_create */

/*
 * lexeme_free
 *
 * Frees a lexeme, returning it to the lookaside list
 * if it was dynamically allocated.  It is OK to pass
 * a null pointer or a pointer to a static lexeme; those
 * will simply be ignored, and it simplifies logic elsewhere
 * to allow this.
 */
void
lexeme_free (lexctx_t lctx, lexeme_t *lex)
{
    if (lex == 0) {
        return;
    }
    if (lex->flags & LEX_M_ALLOCATED) {
        string_free(&lex->text);
        memset(lex, 0xdd, sizeof(lexeme_t));
        lex->tq_next = lctx->freepool;
        lctx->freepool = lex;
    }

} /* lexeme_free */

/*
 * lexeme_copy
 *
 * Allocates a new lexeme and copies the important
 * fields over from the original.
 */
lexeme_t *
lexeme_copy (lexctx_t lctx, lexeme_t *orig)
{
    lexeme_t *lex;

    if (orig == 0) {
        return 0;
    }
    lex = lexeme_alloc(lctx, orig->type, orig->text.ptr,
                       (size_t)orig->text.len);
    if (lex == 0) {
        log_signal(lctx->logctx, 0, STC__INTCMPERR, "lexeme_copy");
        return &errlex;
    }
    lex->boundtype = orig->boundtype;
    lex->extra = orig->extra;
    lex->numval = orig->numval;
    lexeme_copypos(lex, orig);
    lex->tq_next = 0;
    return lex;

} /* lexeme_copy */

/*
 * lexseq_free
 *
 * Frees a entire tail queue of lexemes.
 */
void
lexseq_free (lexctx_t lctx, lexseq_t *seq)
{
    lexeme_t *lex;
    for (lex = lexseq_remhead(seq); lex != 0; lex = lexseq_remhead(seq)) {
        lexeme_free(lctx, lex);
    }
} /* lexseq_free */

/*
 * lexseq_copy
 *
 * Returns a duplicate of a lexeme sequence,
 * appended to the destination.
 */
int
lexseq_copy (lexctx_t lctx, lexseq_t *dst, lexseq_t *src)
{
    lexeme_t *lex;

    for (lex = lexseq_head(src); lex != 0; lex = lexeme_next(lex)) {
        lexseq_instail(dst, lexeme_copy(lctx, lex));
    }

    return 1;

} /* lexseq_copy */

/*
 * lexseq_copy_and_setpos
 *
 * Returns a duplicate of a lexeme sequence,
 * appended to the destination, but resetting the
 * text position for the duplicate to the specified value.
 */
int
lexseq_copy_and_setpos (lexctx_t lctx, lexseq_t *dst, lexseq_t *src,
                        textpos_t pos)
{
    lexeme_t *lex;

    for (lex = lexseq_head(src); lex != 0; lex = lexeme_next(lex)) {
        lexeme_t *copy = lexeme_copy(lctx, lex);
        lexeme_textpos_set(copy, pos);
        lexseq_instail(dst, copy);
    }

    return 1;

} /* lexseq_copy_and_setpos */

/*
 * lexemes_match
 *
 * Compares two sequences of lexemes to see if they are
 * equivalent (e.g., for %IDENTICAL).  That is, the
 * lextypes match and their strings match.
 */
int
lexemes_match (lexseq_t *a, lexseq_t *b)
{   lexeme_t *la, *lb;
    if (lexseq_length(a) != lexseq_length(b)) {
        return 0;
    }
    for (la = lexseq_head(a), lb = lexseq_head(b); la != 0;
         la = la->tq_next, lb = lb->tq_next) {
        if (!strings_eql(&la->text, &lb->text)) {
            return 0;
        }
    }

    return 1;

} /* lexemes_match */
