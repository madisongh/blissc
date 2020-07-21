/*
 *++
 * lexeme.c - Lexeme handling
 *
 * This module implements the basic handling of lexemes.
 * It provides memory management of the lexeme pool, routines
 * for manipulating lexemes and lexeme sequences (along with the
 * inlined functions in the counterpart header file), and
 * the plug-in framework for lexical binding that the parser and other
 * modules use.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdlib.h>
#include "blissc/lexeme.h"

#define DOLEXTYPE(lt) "LEXTYPE_" #lt,
static const char *ltnames[] = { DOLEXTYPES };
#undef DOLEXTYPE

#define ALLOC_QTY 512

struct extenthdr_s {
    struct extenthdr_s *next;
};

struct lexctx_s {
    logctx_t             logctx;
    strctx_t             strctx;
    struct extenthdr_s  *extents;
    lexeme_t            *freepool;
    lextype_bind_fn      binders[LEXTYPE_COUNT];
    void                *bctx[LEXTYPE_COUNT];
};

static lexeme_t errlex = { .type = LEXTYPE_NONE };

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
lextype_register (lexctx_t lctx, void *ctx, lextype_t lt, lextype_bind_fn binder)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return 0;
    }
    lctx->binders[lt] = binder;
    lctx->bctx[lt] = ctx;
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
        struct extenthdr_s *extent;
        extent = malloc(sizeof(struct extenthdr_s) + (ALLOC_QTY * sizeof(lexeme_t)));
        if (extent == 0) {
            log_signal(lctx->logctx, 0, STC__OUTOFMEM, "lexeme_alloc");
            return &errlex;
        }
        extent->next = lctx->extents;
        lctx->extents = extent;
        lctx->freepool = (lexeme_t *)(extent + 1);
        for (i = 0, lex = lctx->freepool; i < ALLOC_QTY-1; i++, lex++) {
            lex->tq_next = lex + 1;
        }
        lex->tq_next = 0;
    }

    lex = lctx->freepool;
    lctx->freepool = lex->tq_next;
    memset(lex, 0, sizeof(lexeme_t));
    string_alloc(lctx->strctx, &lex->text, len);
    if (text != 0) {
        string_from_chrs(lctx->strctx, &lex->text, text, len);
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
 * -1: error
 * 0: binding/unbinding occurred normally, original
 * lexeme modified accordingly
 * 1: binding/unbinding resulted in modification of
 * lexeme sequence (could be null result)
 */
int
lexeme_bind (lexctx_t lctx, textpos_t curpos, quotelevel_t ql, quotemodifier_t qm,
             condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    lextype_t lt = lexeme_boundtype(lex);

    if (lctx->binders[lt] != 0) {
        return lctx->binders[lt](lctx, lctx->bctx[lt], ql, qm, lt, cs, lex, result);
    }

    // Check for lexical conditional skips
    if ((cs == COND_CWA && !(lt == LEXTYPE_LXF_ELSE || lt == LEXTYPE_LXF_FI ||
                             lt == LEXTYPE_LXF_IF)) ||
        (cs == COND_AWC && !(lt == LEXTYPE_LXF_FI || lt == LEXTYPE_LXF_IF))) {
        lexeme_free(lctx, lex);
        return 1;
    }

    // Check for special 'macro skip' - which skips everything except
    // the macro-end marker and terminating of conditionals (the check for
    // which is above).
    if (ql == QL_MACROSKIP && cs == COND_NORMAL && lt != LEXTYPE_MACROEND ) {
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

    if (lt == LEXTYPE_CSTRING) {
        strdesc_t *ltext = lexeme_text(lex);
        strdesc_t *cstr;
        lexeme_t *nlex;
        size_t len;

        len = ltext->len > 255 ? 255 : ltext->len;
        cstr = ascic_string_from_chrs(lctx->strctx, 0, ltext->ptr, len);
        nlex = lexeme_alloc(lctx, lt, cstr->ptr, cstr->len);
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
lexeme_init (strctx_t strctx, logctx_t logctx)
{
    lexctx_t lctx = malloc(sizeof(struct lexctx_s));

    if (lctx != 0) {
        memset(lctx, 0, sizeof(struct lexctx_s));
        lctx->logctx = logctx;
        lctx->strctx = strctx;
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
    struct extenthdr_s *e, *enext;
    if (lctx == 0) {
        return;
    }
    for (e = lctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }

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
        string_free(lctx->strctx, &lex->text);
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
 * lexseq_sersize
 *
 * Computes the number of bytes required to serialize a
 * lexeme sequence.
 */
unsigned int
lexseq_sersize (lexseq_t *seq)
{
    lexeme_t *lex;
    unsigned int count = 0;

    for (lex = lexseq_head(seq); lex != 0; lex = lexeme_next(lex)) {
        strdesc_t *str = lexeme_text(lex);
        // type, boundtype, string length, string
        count += sizeof(uint16_t) + sizeof(uint16_t) +
                 sizeof(uint16_t) + str->len;
    }
    return count;

} /* lexseq_sersize */

/*
 * lexseq_serialize
 *
 * Serialize a lexeme sequence.
 */
int
lexseq_serialize (filectx_t fh, lexseq_t *seq)
{
    lexeme_t *lex;
    uint16_t hdr[3];
    for (lex = lexseq_head(seq); lex != 0; lex = lexeme_next(lex)) {
        strdesc_t *str = lexeme_text(lex);
        hdr[0] = lexeme_type(lex);
        hdr[1] = lexeme_boundtype(lex);
        hdr[2] = (uint16_t) str->len;
        if (file_writebuf(fh, hdr, sizeof(hdr)) < 0) {
            return 0;
        }
        if (file_writebuf(fh, str->ptr, str->len) < 0) {
            return 0;
        }
    }
    return 1;
    
} /* lexseq_serialize */

/*
 * lexseq_deserialize
 *
 * Reconstitute a serialized lexeme sequence.
 */
int
lexseq_deserialize (lexctx_t lctx, filectx_t fh, unsigned int sersize, lexseq_t *seq)
{
    lexeme_t *lex;
    uint16_t hdr[3];
    char buf[256], *b;
    size_t len;

    while (sersize > 0) {
        if (file_readbuf(fh, hdr, sizeof(hdr), &len) <= 0
            || len != sizeof(hdr)) {
            return 0;
        }
        if (hdr[2] > sizeof(buf)) {
            b = malloc(hdr[2]);
            if (b == 0) return 0;
        } else {
            b = buf;
        }
        if (file_readbuf(fh, b, hdr[2], &len) <= 0
            || len != hdr[2]) {
            if (b != buf) free(b);
            return 0;
        }
        lex = lexeme_alloc(lctx, (lextype_t) hdr[0], b, len);
        if (b != buf) free(b);
        lex->boundtype = (lextype_t) hdr[1];
        lexseq_instail(seq, lex);
        sersize -= sizeof(uint16_t) * 3 + hdr[2];
    }

    return 1;
    
} /* lexseq_deserialize */

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
