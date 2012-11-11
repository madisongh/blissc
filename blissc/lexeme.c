//
//  lexeme.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "lexeme.h"
#include "strings.h"

#define DOLEXTYPE(lt) "LEXTYPE_" #lt,
static const char *ltnames[] = { DOLEXTYPES };
#undef DOLEXTYPE

#define ALLOC_QTY 128

static lexeme_t *freepool = 0;
static lexeme_t errlex = { 0, LEXTYPE_NONE };

const char *
lextype_name (lextype_t lt)
{
    if (lt < LEXTYPE_MIN || lt > LEXTYPE_MAX) {
        return "*LEXTYPE_OUTOFRANGE*";
    }
    return ltnames[lt];
}

lexeme_t *
lexeme_alloc (lextype_t type)
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
    string_alloc(&lex->text, 0);
    lex->type = type;
    return lex;
}

void
lexeme___free (lexeme_t *lex)
{
    if (lex != &errlex) {
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
    lex = lexeme_alloc(0);
    if (lex == 0) {
        /* XXX error condition */
        return &errlex;
    }
    memcpy(lex, orig, sizeof(lexeme_t));
    lex->next = 0;
    return lex;
}