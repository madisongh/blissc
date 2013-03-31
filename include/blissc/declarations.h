#ifndef declarations_h__
#define declarations_h__
/*
 *++
 * declarations.h - Definitions for declarations.
 *
 * Copyright © 2012-2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "structures.h"
#include "expression.h"
#include "nametable.h"
#include "parser.h"
#include "lexeme.h"
#include "machinedef.h"

struct declctx_s;
typedef struct declctx_s *declctx_t;

declctx_t declarations_init(expr_ctx_t ctx, parse_ctx_t pctx,
                            scopectx_t kwdscope, machinedef_t *mach);
void declarations_finish(declctx_t dctx);
name_t *define_plit(expr_ctx_t ctx, lextype_t curlt, textpos_t pos);
int parse_decl_name(parse_ctx_t pctx, strdesc_t **result, textpos_t *pos);
int parse_declaration(expr_ctx_t ectx);
int declare_module(expr_ctx_t ectx);
int parse_libgen_declarations(expr_ctx_t ectx);

#endif /* declarations_h__ */
