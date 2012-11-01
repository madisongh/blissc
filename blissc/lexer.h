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

struct lexer_ctx_s;
typedef struct lexer_ctx_s *lexer_ctx_t;

lexer_ctx_t lexer_init(const char *fname, size_t fnlen);
void lexer_finish(lexer_ctx_t ctx);
lexeme_t *lexer_next(lexer_ctx_t ctx, scopectx_t scope, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lexchain);
int lexer_newfile(lexer_ctx_t ctx, const char *fname, size_t fnlen);
lexeme_t *lexeme_create(lexer_ctx_t ctx, scopectx_t scope,
                        lextype_t type, const char *tok, size_t len);
void lexeme_free(lexeme_t *lex);

#endif
