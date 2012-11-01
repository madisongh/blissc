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
typedef int (*parser_dispatch_t)(parse_ctx_t);

parse_ctx_t parser_init(scopectx_t mainscope, void *cctx);
int parser_fopen(parse_ctx_t pctx, const char *fname, size_t fnlen);
void parser_finish(parse_ctx_t pctx);
void *parser_get_cctx(parse_ctx_t pctx);
lexeme_t *parser_next(parse_ctx_t pctx);
void parser_insert(parse_ctx_t pctx, lexeme_t *lex);
void parser_skip_to_delim(parse_ctx_t pctx, lextype_t delimtype);

#endif
