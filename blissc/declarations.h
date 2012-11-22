//
//  declarations.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_declarations__h
#define blissc_declarations__h

#include "nametable.h"
#include "storage.h"
#include "parser.h"
#include "lexeme.h"

void declarations_init(scopectx_t kwdscope);
seg_t *define_plit(parse_ctx_t pctx, stgctx_t stg, lextype_t curlt);
int parse_declaration(parse_ctx_t pctx, lextype_t curlt);
int declare_module(parse_ctx_t pctx);
#endif
