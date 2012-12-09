//
//  macros.h
//  blissc
//
//  Created by Matthew Madison on 11/6/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_macros_h
#define blissc_macros_h

#include "nametable.h"
#include "parser.h"
#include "lexeme.h"

void macros_init(scopectx_t scope);
int macro_paramlist(parse_ctx_t pctx, scopectx_t curscope, int assign_allowed,
                    int for_macro, lextype_t closers[], int nclosers,
                    scopectx_t *ptable, namereflist_t *plist);
int declare_macro(parse_ctx_t pctx, scopectx_t scope, lextype_t curlt);
lexseq_t *macparam_lexseq(name_t *np);
name_t *macparam_special(scopectx_t scope, strdesc_t *pname, lexseq_t *seqval);
name_t *macparam_lookup(scopectx_t scope, strdesc_t *pname, lexseq_t *value);
#endif
