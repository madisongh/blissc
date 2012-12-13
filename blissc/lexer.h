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
#include "scanner.h"
#include "strings.h"

struct lexer_ctx_s;
typedef struct lexer_ctx_s *lexer_ctx_t;

lexer_ctx_t lexer_init(scopectx_t kwdscope);
lexctx_t lexer_lexctx(lexer_ctx_t lctx);
strdesc_t *lexer_filename(lexer_ctx_t lctx, int filename_index);
int lexer_fopen(lexer_ctx_t ctx, const char *fname, size_t fnlen);
int lexer_popen(lexer_ctx_t ctx, scan_input_fn infn, void *fnctx);
void lexer_finish(lexer_ctx_t ctx);
lexeme_t *lexer_next(lexer_ctx_t ctx, int erroneof);
lexeme_t *lexer_peek(lexer_ctx_t ctx, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lex);
void lexer_insert_seq(lexer_ctx_t ctx, lexseq_t *seq);
#endif
