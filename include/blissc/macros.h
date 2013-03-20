#ifndef macros_h__
#define macros_h__
/*
 *++
 * macros.h - Definitions for macros.
 *
 * Copyright © 2012, 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "expression.h"
#include "nametable.h"
#include "parser.h"
#include "lexeme.h"

struct macroctx_s;
typedef struct macroctx_s *macroctx_t;

macroctx_t macros_init(scopectx_t scope, expr_ctx_t ctx);
void macros_finish(macroctx_t mctx);
int macro_paramlist(expr_ctx_t ctx, scopectx_t curscope, int assign_allowed,
                    int for_macro, lextype_t closers[], int nclosers,
                    scopectx_t *ptable, namereflist_t *plist);
int declare_macro(expr_ctx_t ctx, scopectx_t scope, lextype_t curlt);
lexseq_t *macparam_lexseq(name_t *np);
name_t *macparam_special(scopectx_t scope, strdesc_t *pname, lexseq_t *seqval);
name_t *macparam_lookup(lexctx_t lctx, scopectx_t scope,
                        strdesc_t *pname, lexseq_t *value);

#endif /* macros_h__ */
