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

#undef DOLEXTYPE
#define DOLEXTYPES \
    DOLEXTYPE(NONE) \
    DOLEXTYPE(IDENT) \
    DOLEXTYPE(NUMERIC) \
    DOLEXTYPE(STRING) \
    DOLEXTYPE(CSTRING) \
    DOLEXTYPE(END) \
    DOLEXTYPE(OP_ADD) DOLEXTYPE(OP_SUB) DOLEXTYPE(OP_MUL) DOLEXTYPE(OP_DIV) \
    DOLEXTYPE(OP_MOD) \
    DOLEXTYPE(OP_ASSIGN) DOLEXTYPE(OP_FETCH) \
    DOLEXTYPE(OP_SHIFT) \
    DOLEXTYPE(OP_AND) DOLEXTYPE(OP_EQV) DOLEXTYPE(OP_OR) \
    DOLEXTYPE(OP_NOT) DOLEXTYPE(OP_XOR) \
    DOLEXTYPE(OP_EQL) DOLEXTYPE(OP_EQLU) DOLEXTYPE(OP_EQLA) \
    DOLEXTYPE(OP_GEQ) DOLEXTYPE(OP_GEQU) DOLEXTYPE(OP_GEQA) \
    DOLEXTYPE(OP_GTR) DOLEXTYPE(OP_GTRU) DOLEXTYPE(OP_GTRA) \
    DOLEXTYPE(OP_LSS) DOLEXTYPE(OP_LSSU) DOLEXTYPE(OP_LSSA) \
    DOLEXTYPE(OP_LEQ) DOLEXTYPE(OP_LEQU) DOLEXTYPE(OP_LEQA) \
    DOLEXTYPE(OP_NEQ) DOLEXTYPE(OP_NEQU) DOLEXTYPE(OP_NEQA) \
    DOLEXTYPE(DELIM_COMMA) \
    DOLEXTYPE(DELIM_SEMI) \
    DOLEXTYPE(DELIM_COLON) \
    DOLEXTYPE(DELIM_LPAR) \
    DOLEXTYPE(DELIM_RPAR) \
    DOLEXTYPE(DELIM_LBRACK) \
    DOLEXTYPE(DELIM_RBRACK) \
    DOLEXTYPE(DELIM_LANGLE) \
    DOLEXTYPE(DELIM_RANGLE) \
    DOLEXTYPE(DELIM_PERCENT) \
    DOLEXTYPE(DELIM_PTHEN)

#define DOLEXTYPE(lt) LEXTYPE_##lt,
typedef enum {
    DOLEXTYPES
    LEXTYPE_COUNT           // MUST BE LAST
} lextype_t;
#undef DOLEXTYPE

#define LEXTYPE_MIN LEXTYPE_IDENT
#define LEXTYPE_MAX (LEXTYPE_COUNT-1)

static inline int __unused
lex_is_operator (lextype_t lt)
{
    return (lt >= LEXTYPE_OP_ADD && lt <= LEXTYPE_OP_NEQA);
}

static inline int __unused
lex_is_delimiter (lextype_t lt)
{
    return (lt >= LEXTYPE_DELIM_COMMA &&
            lt <= LEXTYPE_DELIM_RANGLE); // just normal delimiters, not lexical
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
const char *lextype_name(lextype_t lt);

#endif
