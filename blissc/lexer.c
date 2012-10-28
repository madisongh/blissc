//
//  lexer.c
//  blissc
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
#include "nametable.h"

struct lexchain_s {
    struct lexchain_s   *nextchain;
    lexeme_t            *head;
    scanctx_t            sctx;
};
typedef struct lexchain_s lexchain_t;

struct lexer_ctx_s {
    lexchain_t   *chain;
};

static const char operators[] = "+-*/=.^";
static lextype_t opertypes[] = {
    LEXTYPE_OP_ADD, LEXTYPE_OP_SUB, LEXTYPE_OP_MUL,
    LEXTYPE_OP_DIV, LEXTYPE_OP_ASSIGN, LEXTYPE_OP_FETCH,
    LEXTYPE_OP_SHIFT
};

static const char delimiters[] = ",;:()[]<>";
static lextype_t delimtypes[] = {
    LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COLON,
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_RPAR,
    LEXTYPE_DELIM_LBRACK, LEXTYPE_DELIM_RBRACK,
    LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_RANGLE
};

static lexchain_t *
lexchain_alloc (void)
{
    lexchain_t *chain = malloc(sizeof(struct lexchain_s));
    if (chain != 0) {
        memset(chain, 0, sizeof(struct lexchain_s));
    }
    return chain;
}

static void
lexchain_free (lexchain_t *chain)
{
    lexeme_t *lex;
    if (chain != 0) {
        for (lex = chain->head; lex != 0; lex = chain->head) {
            chain->head = lex->next;
            lexeme_free(lex);
        }
        if (chain->sctx != 0) {
            scan_finish(chain->sctx);
        }
        free(chain);
    }
}

lexer_ctx_t
lexer_init (const char *fname, size_t fnlen)
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
}

void
lexer_finish (lexer_ctx_t ctx)
{
    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        ctx->chain = chain->nextchain;
        while (chain->head != 0) {
            lexeme_t *lex = chain->head;
            chain->head = lex->next;
            lexeme_free(lex);
        }
        if (chain->sctx != 0) {
            scan_finish(chain->sctx);
        }
    }
    free(ctx);
}

static lexeme_t *
makelex (lexer_ctx_t ctx, scopectx_t scope,
         scantype_t type, char *tok, size_t len)
{
    lexeme_t lex;
    name_t *np;
    char *cp;

    memset(&lex, 0, sizeof(lex));

    switch (type) {
        case SCANTYPE_END:
            lex.type = LEXTYPE_END;
            break;
        case SCANTYPE_DECLITERAL:
            lex.type = LEXTYPE_NUMERIC;
            errno = 0;
            lex.data.val = strtol(tok, &cp, 10);
            if (errno != 0) {
                /* XXX error condition */
            }
            break;
        case SCANTYPE_QUOTEDSTRING:
            lex.type = LEXTYPE_STRING;
            lex.data.ptr = malloc(len+1);
            if (lex.data.ptr != 0) {
                memcpy(lex.data.ptr, tok, len);
                *((char *)lex.data.ptr+len) = '\0';
            } else {
                /* XXX error condition */
            }
            break;
        case SCANTYPE_OPERATOR:
            cp = strchr(operators, *tok);
            lex.type = opertypes[cp-tok];
            break;
        case SCANTYPE_PUNCTUATION:
            cp = strchr(delimiters, *tok);
            lex.type = delimtypes[cp-tok];
            break;
        case SCANTYPE_IDENTIFIER:
            lex.data.ptr = np = name_search(scope, tok, len);
            lex.type = ((np->nameflags & NAME_M_RSVDKWD) ?
                        LEXTYPE_KEYWORD : LEXTYPE_NAME);
            break;
        default:
            break;
    }
    if (lex.type != 0) {
        return lexeme_copy(&lex);
    }
    return 0;
}

lexeme_t *
lexer_next (lexer_ctx_t ctx, scopectx_t scope, int erroneof)
{
    lexeme_t *lex = 0;
    char tokbuf[65];
    size_t len;

    if (ctx == 0)
        return 0;

    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        if (chain->head != 0) {
            lex = chain->head;
            chain->head = lex->next;
            break;
        }
        if (chain->sctx != 0) {
            scantype_t type;
            type = scan_getnext(chain->sctx, tokbuf,
                                sizeof(tokbuf)-1, &len,
                                (erroneof ? SCAN_M_ERRONEOF : 0));
            if (!scan_ok(type)) {
                /* XXX error condition */
                break;
            }
            tokbuf[len] = '\0';
            lex = makelex(ctx, scope, type, tokbuf, len);
            break;
        }
    }
    return lex;
}

// Insert a chain of lexemes at the front of the
// current stream.  If the current stream has no
// chain (file context only), just stick it there.
// Otherwise, allocate a new chain and push it on
// top, so we don't have to walk the list of lexemes.
void
lexer_insert (lexer_ctx_t ctx, lexeme_t *lexchain)
{
    lexchain_t *chain = ctx->chain;

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
}