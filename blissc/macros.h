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

struct macparam_s {
    struct macparam_s *next;
    name_t *np;
};
typedef struct macparam_s macparam_t;

void macros_init(scopectx_t scope);
int macro_paramlist(parse_ctx_t pctx, scopectx_t curscope, int assign_allowed,
                    int for_macro, lextype_t closers[], int nclosers,
                    scopectx_t *ptable, macparam_t **plist, int *pcount);
int declare_macro(parse_ctx_t pctx, scopectx_t scope, lextype_t curlt);
void macparams_free(macparam_t *plist);

#endif
