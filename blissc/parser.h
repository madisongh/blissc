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

int parser_run(scopectx_t initscope,
               const char *fname, size_t fnlen,
               int is_lib);
lexeme_t *parser_next(parse_ctx_t);

#endif
