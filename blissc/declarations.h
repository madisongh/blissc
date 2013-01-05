#ifndef declarations_h__
#define declarations_h__
/*
 *++
 *	File:			declarations.h
 *
 *	Abstract:		Definitions for declarations.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "structures.h"
#include "expression.h"
#include "nametable.h"
#include "parser.h"
#include "lexeme.h"
#include "machinedef.h"

void declarations_init(expr_ctx_t ctx, parse_ctx_t pctx,
                       scopectx_t kwdscope, machinedef_t *mach);
name_t *define_plit(expr_ctx_t ctx, lextype_t curlt, textpos_t pos);
int parse_decl_name(parse_ctx_t pctx, strdesc_t **result, textpos_t *pos);
int parse_declaration(expr_ctx_t ectx);
int declare_module(expr_ctx_t ectx);

#endif /* declarations_h__ */
