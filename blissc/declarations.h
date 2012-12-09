//
//  declarations.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_declarations__h
#define blissc_declarations__h

#include "structures.h"
#include "expression.h"
#include "nametable.h"
#include "storage.h"
#include "parser.h"
#include "lexeme.h"
#include "machinedef.h"

void declarations_init(expr_ctx_t ctx, parse_ctx_t pctx,
                       scopectx_t kwdscope, stgctx_t stg,
                       machinedef_t *mach);
name_t *define_plit(expr_ctx_t ctx, lextype_t curlt, textpos_t pos);
int parse_decl_name(parse_ctx_t pctx, scopectx_t scope,
                    strdesc_t **result, textpos_t *pos);
int parse_declaration(expr_ctx_t ectx);
int declare_module(expr_ctx_t ectx);

#endif
