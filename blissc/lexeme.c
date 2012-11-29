//
//  lexeme.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "lexeme.h"
#include "strings.h"

#define DOLEXTYPE(lt) "LEXTYPE_" #lt,
static const char *ltnames[] = { DOLEXTYPES };
#undef DOLEXTYPE

#define ALLOC_QTY 128

static lexeme_t *freepool = 0;
static lexeme_t errlex = { 0, LEXTYPE_NONE };

static lextype_bind_fn binders[LEXTYPE_COUNT] = { 0 };

const char *
lextype_name (lextype_t lt)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return "*LEXTYPE_OUTOFRANGE*";
    }
    return ltnames[lt];
}

int
lextype_register (lextype_t lt, lextype_bind_fn binder)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return 0;
    }
    binders[lt] = binder;
    return 1;
}

/*
 * lexeme_bind
 *
 * Returns:
 *  -1: error
 *   0: binding/unbinding occurred normally, original
 *      lexeme modified accordingly
 *   1: binding/unbinding resulted in modification of
 *      lexeme sequence (could be null result)
 */
int
lexeme_bind (void *ctx, quotelevel_t ql, quotemodifier_t qm,
             condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    lextype_t lt = lexeme_boundtype(lex);

    if (binders[lt] != 0) {
        return binders[lt](ctx, ql, qm, lt, cs, lex, result);
    }

    // Check for lexical conditional skips
    if ((cs == COND_CWA && !(lt == LEXTYPE_LXF_ELSE || lt == LEXTYPE_LXF_FI)) ||
        (cs == COND_AWC && lt != LEXTYPE_LXF_FI)) {
        lexeme_free(lex);
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
            /* XXX error condition */
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
        nlex = lexeme_alloc(lt, cstr->ptr, cstr->len);
        lexeme_copypos(nlex, lex);
        lexseq_instail(result, nlex);
        lexeme_free(lex);
        return 1;
    }
    lex->type = lt;
    return 0;
}

lexeme_t *
lexeme_alloc (lextype_t type, const char *text, size_t len)
{
    lexeme_t *lex;
    int i;

    if (freepool == 0) {
        freepool = malloc(ALLOC_QTY * sizeof(lexeme_t));
        if (freepool == 0) {
            /* XXX error condition */
            return &errlex;
        }
        for (i = 0, lex = freepool; i < ALLOC_QTY-1; i++, lex++) {
            lex->next = lex + 1;
        }
        lex->next = 0;
    }

    lex = freepool;
    freepool = lex->next;
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
 * lexeme_create
 *
 * Public API for creating a lexeme from some value, used
 * by the lexical analyzer when expanding lexical functions
 * and macros.
 */
lexeme_t *
lexeme_create (lextype_t type, strdesc_t *tok)
{
    lexeme_t *lex;

    if (type < LEXTYPE_MIN || type > LEXTYPE_MAX) {
        /* XXX error condition */
        return &errlex;
    }
    lex  = lexeme_alloc(LEXTYPE_UNBOUND, tok->ptr, tok->len);
    if (lex->type == LEXTYPE_NONE) {
        return &errlex;
    }
    lex->boundtype = type;

    return lex;

} /* lexeme_create */



void
lexeme_free (lexeme_t *lex)
{
    if (lex == 0) {
        return;
    }
    if (lex->flags & LEX_M_ALLOCATED) {
        string_free(&lex->text);
        memset(lex, 0xdd, sizeof(lexeme_t));
        lex->next = freepool;
        freepool = lex;
    }
}

lexeme_t *
lexeme_copy (lexeme_t *orig)
{
    lexeme_t *lex;

    if (orig == 0) {
        return 0;
    }
    lex = lexeme_alloc(orig->type, orig->text.ptr, (size_t)orig->text.len);
    if (lex == 0) {
        /* XXX error condition */
        return &errlex;
    }
    lex->boundtype = orig->boundtype;
    lex->extra = orig->extra;
    lex->numval = orig->numval;
    lexeme_copypos(lex, orig);
    lex->next = 0;
    return lex;
}

/*
 * lexseq_free
 *
 * Frees the linked list of lexemes (the 'sequence')
 * ponted to by 'seq'.
 */
void
lexseq_free (lexseq_t *seq)
{
    lexeme_t *lex;
    for (lex = lexseq_remhead(seq); lex != 0; lex = lexseq_remhead(seq)) {
        lexeme_free(lex);
    }
} /* lexseq_free */

/*
 * lexseq_copy
 *
 * Returns a duplicate of a lexeme sequence.
 */
int
lexseq_copy (lexseq_t *dst, lexseq_t *src)
{
    lexeme_t *lex;

    for (lex = lexseq_head(src); lex != 0; lex = lexeme_next(lex)) {
        lexseq_instail(dst, lexeme_copy(lex));
    }

    return 1;

} /* lexseq_copy */

/*
 * lexemes_match
 *
 * Compares two sequences of lexemes to see if they are
 * equivalent (e.g., for %IDENTICAL).  That is, the
 * lextypes match, and for lextypes for which there
 * is data, that data matches.
 */
int
lexemes_match (lexseq_t *a, lexseq_t *b)
{   lexeme_t *la, *lb;
    if (lexseq_length(a) != lexseq_length(b)) {
        return 0;
    }
    for (la = lexseq_head(a), lb = lexseq_head(b); la != 0;
         la = la->next, lb = lb->next) {
        if (!strings_eql(&la->text, &lb->text)) {
            return 0;
        }
    }

    return 1;

} /* lexemes_match */
