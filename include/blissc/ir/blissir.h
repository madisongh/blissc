#ifndef blissir_h__
#define blissir_h__
/*
 *++
 *  blissir.h - BLISS IR definitions
 *
 *  Copyright © 2014, Matthew Madison
 *  All rights reserved.
 *  Distributed under license.  See LICENSE.TXT for details.
 *--
 */

#include "symbols.h"
#include "nametable.h"
#include "machinedef.h"
#include "support/strings.h"

struct blissir_ctx_s;
typedef struct blissir_ctx_s *blissir_ctx_t;

struct blissir_module_s;
typedef struct blissir_module_s *blissir_moduleref_t;

struct blissir_routine_s;
typedef struct blissir_routine_s *blissir_routineref_t;

struct blissir_basicblock_s;
typedef struct blissir_basicblock_s *blissir_basicblockref_t;

struct blissir_value_s;
typedef struct blissir_value_s *blissir_valueref_t;

typedef enum {
    VALTYPE_LITERAL,
    VALTYPE_SEGADDR,
    VALTYPE_REGISTER,
} blissir_valtype_t;

struct blissir_builder_s;
typedef struct blissir_builder_s *blissir_builderref_t;

blissir_ctx_t blissir_init(machinedef_t *mach);
blissir_moduleref_t blissir_module_create(blissir_ctx_t ctx, name_t *np);

blissir_routineref_t blissir_routine_create(blissir_ctx_t ctx, name_t *np,
                                            routine_attr_t *attr);
blissir_routineref_t blissir_routine_find(blissir_ctx_t ctx, name_t *np);
int blissir_routine_begin(blissir_ctx_t ctx, name_t *np);
int blissir_routine_end(blissir_routineref_t rtn);

blissir_basicblockref_t blissir_basicblock_append(blissir_routineref_t rtn,
                                                  strdesc_t *label);
blissir_basicblockref_t blissir_basicblock_insert(blissir_routineref_t rtn,
                                                  blissir_basicblockref_t pred,
                                                  strdesc_t *label);
int blissir_basicblock_delete(blissir_basicblockref_t bb);
blissir_basicblockref_t blissir_basicblock_find(blissir_routineref_t rtn,
                                                strdesc_t *label);
blissir_builderref_t blissir_builder_create(blissir_ctx_t ctx,
                                            blissir_basicblockref_t bb);
int blissir_builder_setpos_atend(blissir_builderref_t builder,
                                 blissir_basicblockref_t bb);

#endif /* blissir_h__ */
