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

#define DOLEXTYPE(lt) "LEXTYPE_" #lt,
static const char *ltnames[] = { DOLEXTYPES };
#undef DOLEXTYPE

#define ALLOC_QTY 128

static lexeme_t *freepool = 0;

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
            return 0;
        }
        for (i = 0, lex = freepool; i < ALLOC_QTY-1; i++, lex++) {
            lex->next = lex + 1;
        }
        lex->next = 0;
    }

    lex = freepool;
    freepool = lex->next;
    memset(lex, 0, sizeof(lexeme_t));
    lex->type = type;
    return lex;
}

void
lexeme___free (lexeme_t *lex)
{
    lex->next = freepool;
    freepool = lex;
}

lexeme_t *
lexeme_copy (lexeme_t *orig)
{
    lexeme_t *lex = lexeme_alloc(0);
    if (lex == 0) {
        /* XXX error condition */
        return lex;
    }
    memcpy(lex, orig, sizeof(lexeme_t));
    lex->next = 0;
    return lex;
}