//
//  lexeme.h
//  blissc
//
//  Created by Matthew Madison on 10/23/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_lexeme_h
#define blissc_lexeme_h

#include <string.h>
#include "utils.h"

typedef enum {
    LEXTYPE_IDENT = 1,
    LEXTYPE_NUMERIC,
    LEXTYPE_STRING,
    LEXTYPE_CSTRING,
    LEXTYPE_END,
    LEXTYPE_OP_ADD,
    LEXTYPE_OP_SUB,
    LEXTYPE_OP_MUL,
    LEXTYPE_OP_DIV,
    LEXTYPE_OP_ASSIGN,
    LEXTYPE_OP_FETCH,
    LEXTYPE_OP_SHIFT,
    LEXTYPE_DELIM_COMMA,
    LEXTYPE_DELIM_SEMI,
    LEXTYPE_DELIM_COLON,
    LEXTYPE_DELIM_LPAR,
    LEXTYPE_DELIM_RPAR,
    LEXTYPE_DELIM_LBRACK,
    LEXTYPE_DELIM_RBRACK,
    LEXTYPE_DELIM_LANGLE,
    LEXTYPE_DELIM_RANGLE,
    LEXTYPE_DELIM_PERCENT,
    LEXTYPE_COUNT           // MUST BE LAST
} lextype_t;

#define LEXTYPE_MIN LEXTYPE_IDENT
#define LEXTYPE_MAX (LEXTYPE_COUNT-1)

static inline int __unused
lex_is_operator (lextype_t lt)
{
    return (lt >= LEXTYPE_OP_ADD && lt <= LEXTYPE_OP_SHIFT);
}

static inline int __unused
lex_is_delimiter (lextype_t lt)
{
    return (lt >= LEXTYPE_DELIM_COMMA &&
            lt <= LEXTYPE_DELIM_RANGLE);
}

struct lexeme_s {
    struct lexeme_s *next;
    lextype_t        type;
    data_t           data;
};
typedef struct lexeme_s lexeme_t;

lexeme_t *lexeme_alloc(lextype_t type);
lexeme_t *lexeme_copy(lexeme_t *orig);
void lexeme___free(lexeme_t *lex);

#endif
