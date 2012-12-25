#ifndef structures_h__
#define structures_h__
/*
 *++
 *	File:			structures.h
 *
 *	Abstract:		Definitions for structures
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "expression.h"
#include "nametable.h"
#include "lexeme.h"

struct strudef_s;
typedef struct strudef_s strudef_t;
struct sym_data_s;

void structures_init(expr_ctx_t ctx, scopectx_t kwdscope);
int declare_structure(expr_ctx_t ctx, scopectx_t scope);
int declare_field(expr_ctx_t ctx, scopectx_t scope);
int structure_allocate(expr_ctx_t ctx, name_t *struname,
                       strudef_t **strup, unsigned int *units,
                       scopectx_t *scopep, int is_ref);
expr_node_t *structure_reference(expr_ctx_t pctx, name_t *struname,
                        int ctce_accessors, name_t *segname,
                        lexeme_t *lex);
lexeme_t *field_extract(name_t *fnp, unsigned int which);
lexseq_t *field_lexseq(name_t *fnp);
namereflist_t *fieldset_reflist(name_t *fsnp);

#endif /* structures_h__ */
