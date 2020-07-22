#ifndef lexer_h__
#define lexer_h__
/*
 *++
 * lexer.h - Lexer interface definitions.
 *
 * Copyright © 2012-2020, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "lexeme.h"
#include "nametable.h"
#include "scanner.h"
#include "support/logging.h"
#include "support/strings.h"

struct lexer_ctx_s;
typedef struct lexer_ctx_s *lexer_ctx_t;

lexer_ctx_t lexer_init(strctx_t strctx, scopectx_t kwdscope,
                       logctx_t logctx, void *fioctx);
lexctx_t lexer_lexctx(lexer_ctx_t lctx);
void *lexer_scanctx(lexer_ctx_t ctx);
int lexer_fopen(lexer_ctx_t ctx, const char *fname, size_t fnlen, char **actnamep);
int lexer_popen(lexer_ctx_t ctx, scan_input_fn infn, void *fnctx);
void lexer_finish(lexer_ctx_t ctx);
lexeme_t *lexer_next(lexer_ctx_t ctx, int erroneof, textpos_t *posp);
lexeme_t *lexer_peek(lexer_ctx_t ctx, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lex);
void lexer_insert_seq(lexer_ctx_t ctx, lexseq_t *seq);

#endif /* lexer_h__ */
