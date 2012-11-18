//
//  parser.h
//  blissc
//
//  Created by Matthew Madison on 10/28/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_parser_h
#define blissc_parser_h

#include "nametable.h"
#include "lexeme.h"

struct parse_ctx_s;
typedef struct parse_ctx_s *parse_ctx_t;

parse_ctx_t parser_init(scopectx_t mainscope, void *cctx);
int parser_fopen(parse_ctx_t pctx, const char *fname, size_t fnlen);
void parser_finish(parse_ctx_t pctx);
void *parser_get_cctx(parse_ctx_t pctx);
lextype_t parser_next(parse_ctx_t pctx, quotelevel_t ql, lexeme_t **lex);
void parser_insert(parse_ctx_t pctx, lexeme_t *lex);
void parser_insert_seq(parse_ctx_t pctx, lexseq_t *seq);
void parser_skip_to_delim(parse_ctx_t pctx, lextype_t delimtype);
int parser_decl_ok(parse_ctx_t pctx, scopectx_t *curscope);
scopectx_t parser_scope_get(parse_ctx_t pctx);
void parser_set_declstate(parse_ctx_t pctx, int val);
void parser_incr_erroneof(parse_ctx_t pctx);
void parser_decr_erroneof(parse_ctx_t pctx);
int parse_lexeme_seq(parse_ctx_t pctx, lexseq_t *seq, quotelevel_t ql,
                     lextype_t terms[], int nterms,
                     lexseq_t *result, lextype_t *term);
lexeme_t *parser_lexeme_create(parse_ctx_t pctx, lextype_t lt, strdesc_t *txt);
int parser_expect(parse_ctx_t pctx, quotelevel_t ql,
                  lextype_t explt, lexeme_t **lex, int pboe);
#endif
