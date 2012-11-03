//
//  lexer.c
//  blissc
//
//  Low-level lexeme handling.  Maintains the stream of lexemes
//  used by the parser; lexemes can be inserted into the stream,
//  files can be passed to the scanner routines for reading and
//  tokenizing.
//
//  At this level, only the most basic token-to-lexeme conversion
//  happens -- specifically, conversion of operators and delimiters,
//  and the primitive numeric and string literals.
//
//  This module also has the upper layer of lexeme memory management,
//  with knowledge about the 'data' portion of lexeme types that
//  need to have memory freed.    XXX - may need to revisit this
//
//  Created by Matthew Madison on 10/23/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "lexeme.h"
#include "lexer.h"
#include "scanner.h"
#include "strings.h"

/*
 * This structure is for inserting a new chain of lexemes
 * into the parsing stream.  When lexical processing adds
 * or replaces lexemes in the stream, they are prepended to
 * the linked list pointed to by 'head'.  When files are
 * opened (e.g., REQUIRE), the scanner context is maintained
 * in 'sctx'.
 */
struct lexchain_s {
    struct lexchain_s   *nextchain;
    lexeme_t            *head;
    scanctx_t            sctx;
};
typedef struct lexchain_s lexchain_t;

/*
 * Context for this module.  Contains the linked list of
 * lexchain_t structures that represent the parsing stream.
 * When the end of the stream is reached, 'atend' is set;
 * when set, lexer_next() simply returns LEXTYPE_END as
 * the next lexeme, no matter how many times it is called.
 * This eliminates the need for a bunch of code in the
 * layers above to make sure we don't lose the end-of-stream
 * marker.
 */
struct lexer_ctx_s {
    lexchain_t   *chain;
    int          atend;
};

/*
 * Maps of characters to their respective operator and
 * delimiter lexeme types.
 */
static const char operators[] = "+-*/=.^";
static lextype_t opertypes[] = {
    LEXTYPE_OP_ADD, LEXTYPE_OP_SUB, LEXTYPE_OP_MUL,
    LEXTYPE_OP_DIV, LEXTYPE_OP_ASSIGN, LEXTYPE_OP_FETCH,
    LEXTYPE_OP_SHIFT
};

static const char delimiters[] = ",;:()[]<>%";
static lextype_t delimtypes[] = {
    LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COLON,
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_RPAR,
    LEXTYPE_DELIM_LBRACK, LEXTYPE_DELIM_RBRACK,
    LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_RANGLE,
    LEXTYPE_DELIM_PERCENT
};

/*
 * These static lexemes are used to simplify the
 * logic in the parser.
 */
static lexeme_t errlex = { 0, LEXTYPE_NONE };
static lexeme_t endlex = { 0, LEXTYPE_END };

/*
 * Operators that are keywords.
 */
#define DODEFS \
DODEF(AND) DODEF(EQV) DODEF(OR) \
DODEF(NOT) DODEF(XOR) \
DODEF(EQL) DODEF(EQLU) DODEF(EQLA) \
DODEF(GEQ) DODEF(GEQU) DODEF(GEQA) \
DODEF(GTR) DODEF(GTRU) DODEF(GTRA) \
DODEF(LSS) DODEF(LSSU) DODEF(LSSA) \
DODEF(LEQ) DODEF(LEQU) DODEF(LEQA) \
DODEF(NEQ) DODEF(NEQU) DODEF(NEQA)
#define DODEF(name_) OPRDEF(#name_, LEXTYPE_OP_##name_),

static name_t operator_names[] = {
    DODEFS
};
#undef DODEF
#undef DODEFS


/*
 * -- lexchain memory management --
 */

/*
 * lexchain_alloc
 *
 * Allocate a new lexchain_t.
 */
static lexchain_t *
lexchain_alloc (void)
{
    lexchain_t *chain = malloc(sizeof(struct lexchain_s));
    if (chain != 0) {
        memset(chain, 0, sizeof(struct lexchain_s));
    }
    return chain;

} /* lexchain_alloc */

/*
 * lexchain_free
 *
 * Frees a lexchain_t, along with all of the lexemes on
 * its linked list, and closing the scanner context.
 */
static void
lexchain_free (lexchain_t *chain)
{
    if (chain != 0) {
        if (chain->head != 0) {
            lexseq_free(chain->head);
        }
        if (chain->sctx != 0) {
            scan_finish(chain->sctx);
        }
        free(chain);
    }
} /* lexchain_free */

/* --- Public API --- */

/*
 * lexer_init
 *
 * One-time lexer initialization, to register the operator names.
 */
void
lexer_init (scopectx_t kwdscope)
{
    int i;
    for (i = 0; i < sizeof(operator_names)/sizeof(operator_names[0]); i++) {
        name_insert(kwdscope, &operator_names[i]);
    }
} /* lexer_init */

/*
 * lexer_fopen
 *
 * Called to begin scanning for lexemes in a file.
 */
lexer_ctx_t
lexer_fopen (const char *fname, size_t fnlen)
{
    lexer_ctx_t ctx = malloc(sizeof(struct lexer_ctx_s));
    if (ctx != 0) {
        
        memset(ctx, 0, sizeof(struct lexer_ctx_s));
        ctx->chain = lexchain_alloc();
        ctx->chain->sctx = scan_init();
        if (ctx->chain->sctx == 0) {
            lexchain_free(ctx->chain);
            free(ctx);
            ctx = 0;
        } else {
            if (!scan_fopen(ctx->chain->sctx,
                           fname, fnlen)) {
                scan_finish(ctx->chain->sctx);
                lexchain_free(ctx->chain);
                free(ctx);
                ctx = 0;
            }
        }
    }
    return ctx;

} /* lexer_fopen */

/*
 * lexer_finish
 *
 * Shuts down the lexer, freeing up all of the lexchains.
 */
void
lexer_finish (lexer_ctx_t ctx)
{
    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        ctx->chain = chain->nextchain;
        lexchain_free(chain);
    }
    free(ctx);

} /* lexer_finish */

/*
 * lexeme_create
 *
 * Public API for creating a lexeme from some value, used
 * by the lexical analyzer when expanding lexical functions
 * and macros.
 */
lexeme_t *
lexeme_create (lexer_ctx_t ctx, scopectx_t scope,
               lextype_t type, const char *tok, size_t len)
{
    lexeme_t *lex;
    name_t *np;
    char *cp;

    if (type < LEXTYPE_MIN || type > LEXTYPE_MAX) {
        /* XXX error condition */
        return &errlex;
    }
    lex  = lexeme_alloc(type);
    if (lex->type == LEXTYPE_NONE) {
        return &errlex;
    }
    switch (type) {
        case LEXTYPE_NUMERIC:
            errno = 0;
            lex->data.val_signed = strtol(tok, &cp, 10);
            if (errno != 0) {
                /* XXX error condition */
            }
            break;
        case LEXTYPE_STRING:
            if (string_from_chrs(&lex->data.val_string, tok, len) == 0) {
                /* XXX error condition */
                lexeme_free(lex);
                return 0;
            }
            break;
        case LEXTYPE_CSTRING:
            if (len > 255) {
                len = 255;
            }
            if (ascic_string_from_chrs(&lex->data.val_string, tok, len) == 0) {
                /* XXX error condition */
                lexeme_free(lex);
                return 0;
            }
            break;
        case LEXTYPE_IDENT:
            np = name_search(scope, tok, len, 1);
            if (np->nametype == NAMETYPE_KEYWORD &&
                (np->nameflags & NAME_M_OPERATOR)) {
                type = (lextype_t) np->namedata.val_signed;
            } else {
                lex->data.ptr = np;
            }
            break;
        default:
            break;
    }

    lex->type = type;
    return lex;

} /* lexeme_create */

/*
 * lexeme_free
 *
 * Frees a lexeme.  For lexeme types that have
 * dynamically allocated data, frees that data
 * as well.
 *
 * NOTE that the 'next' field is NOT checked here;
 * it is assumed that the caller has unlinked the
 * lexeme from any linked lists before calling.
 */
void
lexeme_free (lexeme_t *lex)
{
    if (lex == &errlex || lex == &endlex) {
        return;
    }
    switch (lex->type) {
        case LEXTYPE_STRING:
        case LEXTYPE_CSTRING:
            string_free(&lex->data.val_string);
            break;
        case LEXTYPE_IDENT:
            name_free(lex->data.ptr);
            break;
        default:
            break;
    }
    lexeme___free(lex);

} /* lexeme_free */

/*
 * lexseq_free
 *
 * Frees the linked list of lexemes (the 'sequence')
 * ponted to by 'seq'.
 */
void
lexseq_free (lexeme_t *seq)
{
    lexeme_t *lex, *next;
    for (lex = seq; lex != 0; lex = next) {
        next = lex->next;
        lexeme_free(lex);
    }
} /* lexseq_free */

/*
 * lexer_next
 *
 * Returns the next lexeme in the stream.
 * If 'erroneof' is set, reaching the end of a file (not the
 * end of the stream) should be considered an error condition --
 * this happens when we're in the middle of parsing a lexical
 * conditional or a macro definition; they must be wholly contained
 * within a single file.
 */
lexeme_t *
lexer_next (lexer_ctx_t ctx, scopectx_t scope, int erroneof)
{
    lexeme_t *lex = 0;
    lextype_t lextype;
    char tokbuf[65], *cp;
    size_t len;

    if (ctx == 0) {
        return &errlex;
    }

    if (ctx->atend) {
        return &endlex;
    }

    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        if (chain->head != 0) {
            lex = chain->head;
            chain->head = lex->next;
            lex->next = 0;
            break;
        }
        if (chain->sctx != 0) {
            scantype_t type;
            type = scan_getnext(chain->sctx, tokbuf,
                                sizeof(tokbuf)-1, &len,
                                (erroneof ? SCAN_M_ERRONEOF : 0));
            if (!scan_ok(type)) {
                /* XXX error condition */
                lex = &errlex;
                break;
            }
            if (type == SCANTYPE_END) {
                scan_finish(chain->sctx);
                chain->sctx = 0;
                ctx->chain = chain->nextchain;
                lexchain_free(chain);
                if (ctx->chain == 0) {
                    lex = lexeme_alloc(LEXTYPE_END);
                    ctx->atend = 1;
                    break;
                }
                continue;
            }
            tokbuf[len] = '\0';
            switch (type) {
                case SCANTYPE_DECLITERAL:
                    lextype = LEXTYPE_NUMERIC;
                    break;
                case SCANTYPE_QUOTEDSTRING:
                    lextype = LEXTYPE_STRING;
                    break;
                case SCANTYPE_OPERATOR:
                    cp = strchr(operators, tokbuf[0]);
                    lextype = opertypes[cp-operators];
                    break;
                case SCANTYPE_PUNCTUATION:
                    cp = strchr(delimiters, tokbuf[0]);
                    lextype = delimtypes[cp-delimiters];
                    break;
                case SCANTYPE_IDENTIFIER:
                    lextype = LEXTYPE_IDENT;
                    break;
                default:
                    break;
            }
            lex = (lextype == 0 ? &errlex
                   : lexeme_create(ctx, scope, lextype, tokbuf, len));
            break;
        }
    }
    return lex;

} /* lexer_next */

/*
 * lexer_insert
 *
 * Insert a chain of lexemes at the front of the
 * current stream.  If the current stream has no
 * chain (file context only), just stick it there.
 * Otherwise, allocate a new chain and push it on
 * top, so we don't have to walk the list of lexemes.
 */
void
lexer_insert (lexer_ctx_t ctx, lexeme_t *lexchain)
{
    lexchain_t *chain = ctx->chain;

    // The static ones should never be inserted.
    if (lexchain == &errlex || lexchain == &endlex) {
        return;
    }
    
    if (chain == 0 || chain->head != 0) {
        chain = lexchain_alloc();
        if (chain == 0) {
            /* XXX error condition */
            return;
        }
        chain->nextchain = ctx->chain;
        ctx->chain = chain;
    }
    chain->head = lexchain;

} /* lexer_insert */

/*
 * lexer_newfile
 * Insert a new file at the front of the stream (for REQUIRE and %REQUIRE).
 */
int
lexer_newfile (lexer_ctx_t ctx, const char *fname, size_t fnlen)
{
    lexchain_t *chain = lexchain_alloc();

    if (chain == 0) {
        /* XXX error condition */
        return 0;
    }
    chain->sctx = scan_init();
    if (chain->sctx == 0) {
        lexchain_free(chain);
        return 0;
    }
    if (!scan_fopen(chain->sctx, fname, fnlen)) {
        scan_finish(chain->sctx);
        chain->sctx = 0;
        lexchain_free(chain);
        return 0;
    }
    chain->nextchain = ctx->chain;
    ctx->chain = chain;
    return 1;

} /* lexer_newfile */

/*
 * lexemes_match
 *
 * Compares two sequences of lexemes to see if they are
 * equivalent (e.g., for %IDENTICAL).  That is, the
 * lextypes match, and for lextypes for which there
 * is data, that data matches.
 *
 * NB: name-style operators should already have been
 *     resolved to their LEXTYPE_OP_xxx codes (which
 *     lexeme_create() does).
 *
 */
int
lexemes_match (lexeme_t *a, lexeme_t *b)
{
    name_t *an, *bn;

    while (a != 0 && b != 0) {
        if (a->type != b->type) {
            return 0;
        }
        switch (a->type) {
            case LEXTYPE_IDENT:
                an = a->data.ptr;
                bn = b->data.ptr;
                if (an->namelen != bn->namelen) {
                    return 0;
                }
                if (memcmp(an->name, bn->name, an->namelen) != 0) {
                    return 0;
                }
                break;
            case LEXTYPE_STRING:
            case LEXTYPE_CSTRING:
                if (!strings_eql(&a->data.val_string, &b->data.val_string)) {
                    return 0;
                }
                break;
            case LEXTYPE_NUMERIC:
                if (a->data.val_signed != b->data.val_signed) {
                    return 0;
                }
                break;
            default:
                break;
        }
        a = a->next;
        b = b->next;
    }

    return (a == 0 && b == 0);

} /* lexemes_match */