#ifndef lexer_h__
#define lexer_h__
/*
 *++
 *	File:			lexer.h
 *
 *	Abstract:		Lexer interface definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "lexeme.h"
#include "nametable.h"
#include "scanner.h"
#include "logging.h"
#include "strings.h"

struct lexer_ctx_s;
typedef struct lexer_ctx_s *lexer_ctx_t;

lexer_ctx_t lexer_init(strctx_t strctx, scopectx_t kwdscope, logctx_t logctx);
lexctx_t lexer_lexctx(lexer_ctx_t lctx);
strdesc_t *lexer_filename(lexer_ctx_t lctx, int filename_index);
int lexer_fopen(lexer_ctx_t ctx, const char *fname, size_t fnlen,
                const char *suffix);
int lexer_popen(lexer_ctx_t ctx, scan_input_fn infn, void *fnctx);
void lexer_finish(lexer_ctx_t ctx);
lexeme_t *lexer_next(lexer_ctx_t ctx, int erroneof, textpos_t *posp);
lexeme_t *lexer_peek(lexer_ctx_t ctx, int erroneof);
void lexer_insert(lexer_ctx_t ctx, lexeme_t *lex);
void lexer_insert_seq(lexer_ctx_t ctx, lexseq_t *seq);

#endif /* lexer_h__ */
