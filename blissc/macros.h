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

void macros_init(scopectx_t kwdscope);
int macro_expand(parse_ctx_t pctx, scopectx_t curscope, name_t *macroname);

#endif
