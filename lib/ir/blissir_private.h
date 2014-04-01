#ifndef blissir_private_h__
#define blissir_private_h__
/*
 *++
 *  blissir_private.h - Internal BLISS IR definitions
 *
 *  Copyright © 2014, Matthew Madison
 *  All rights reserved.
 *  Distributed under license.  See LICENSE.TXT for details.
 *--
 */
#include "blissc/ir/blissir.h"
#include "blissc/support/utils.h"

struct blissir_value_s {
    blissir_valtype_t   type;

};
typedef struct blissir_value_s blissir_value_t;


struct blissir_basicblock_s {
    TQ_ENT_FIELDS(struct blissir_basicblock_s)
    blissir_routineref_t    routine;
};
typedef struct blissir_basicblock_s blissir_basicblock_t;

typedef struct basicblock_seq_s {
    TQ_HDR_FIELDS(blissir_basicblock_t)
} basicblock_seq_t;
DEFINE_TQ_FUNCS(basicblock_seq, basicblock_seq_t, blissir_basicblock_t)

struct blissir_routine_s {
    TQ_ENT_FIELDS(struct blissir_routine_s)
    blissir_moduleref_t module;
    basicblock_seq_t    basicblocks;
};
typedef struct blissir_routine_s blissir_routine_t;

typedef struct routine_seq_s {
    TQ_HDR_FIELDS(blissir_routine_t)
} routine_seq_t;
DEFINE_TQ_FUNCS(routine_seq, routine_seq_t, blissir_routine_t)

struct blissir_builder_s {
    TQ_ENT_FIELDS(struct blissir_builder_s)
};
typedef struct blissir_builder_s blissir_builder_t;

typedef struct builder_seq_s {
    TQ_HDR_FIELDS(blissir_builder_t)
} builder_seq_t;
DEFINE_TQ_FUNCS(builder_seq, builder_seq_t, blissir_builder_t)

struct blissir_module_s {
    routine_seq_t   routines;
    builder_seq_t   builders;
};
typedef struct blissir_module_s blissir_module_t;

struct blissir_ctx_s {
    blissir_module_t    module;
};


#endif /* blissir_private_h__ */
