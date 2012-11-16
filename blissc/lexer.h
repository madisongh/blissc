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
lexeme_t *lexer_next(lexer_ctx_t ctx, int erroneof);
lexeme_t *lexer_peek(lexer_ctx_t ctx, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lex);
void lexer_insert_seq(lexer_ctx_t ctx, lexseq_t *seq);
int lexer_newfile(lexer_ctx_t ctx, const char *fname, size_t fnlen);
void lexseq_free(lexseq_t *seq);
int lexseq_copy(lexseq_t *dst, lexseq_t *src);
int lexemes_match(lexseq_t *a, lexseq_t *b);
#endif
