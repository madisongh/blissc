//
//  lexer.h
//  blissc
//
//  Created by Matthew Madison on 10/23/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_lexer_h
#define blissc_lexer_h

#include "lexeme.h"
#include "nametable.h"
#include "strings.h"

struct lexer_ctx_s;
typedef struct lexer_ctx_s *lexer_ctx_t;

void lexer_init(scopectx_t kwdscope);
lexer_ctx_t lexer_fopen(const char *fname, size_t fnlen);
void lexer_finish(lexer_ctx_t ctx);
lexeme_t *lexer_next(lexer_ctx_t ctx, scopectx_t scope, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lexchain);
int lexer_newfile(lexer_ctx_t ctx, const char *fname, size_t fnlen);
lexeme_t *lexeme_create(lextype_t type, strdesc_t *dsc);
lexeme_t *lexeme_bind(scopectx_t scope, lexeme_t *lex);
lexeme_t *lexeme_unbind(lexeme_t *lex);
void lexeme_free(lexeme_t *lex);
void lexseq_free(lexeme_t *seq);
int lexemes_match(lexeme_t *a, lexeme_t *b);
name_t *lexeme_nameval(lexeme_t *lex, scopectx_t scope);
strdesc_t *lexeme_stringval(lexeme_t *lex);
long lexeme_signedval(lexeme_t *lex);
unsigned long lexeme_unsignedval(lexeme_t *lex);
#endif
