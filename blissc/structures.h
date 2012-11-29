//
//  structures.h
//  blissc
//
//  Created by Matthew Madison on 11/24/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_structures_h
#define blissc_structures_h

#include "parser.h"
#include "nametable.h"

struct strudef_s;
typedef struct strudef_s strudef_t;

struct fieldset_s;
typedef struct fieldset_s fieldset_t;

void structures_init(void);
int declare_structure(parse_ctx_t pctx, scopectx_t scope);
int structure_allocate(parse_ctx_t pctx, name_t *struname,
                       strudef_t **strup, unsigned int *units,
                       scopectx_t *scopep);
strdesc_t *structure_name(strudef_t *stru);
#endif
