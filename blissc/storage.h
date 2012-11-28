//
//  storage.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_storage_h
#define blissc_storage_h

#include <stdint.h>
#include "machinedef.h"
#include "strings.h"
#include "utils.h"

typedef struct stgctx_s *stgctx_t;


#define PSECT_M_ATTR_WRITE   (1<<0)
#define PSECT_M_ATTR_EXEC    (1<<1)
#define PSECT_M_ATTR_OVERLAY (1<<2)

#define SEG_M_VOLATILE       (1<<0)
#define SEG_M_ALIAS          (1<<1)

#undef DOSEGTYPE
#define DOSEGTYPES \
    DOSEGTYPE(STATIC) DOSEGTYPE(STACK) \
    DOSEGTYPE(LITERAL)
#define DOSEGTYPE(t_) SEGTYPE_##t_,
typedef enum {
    DOSEGTYPES
} segtype_t;
#undef DOSEGTYPE

struct psect_s;
typedef struct psect_s psect_t;
struct frame_s;
typedef struct frame_s frame_t;
struct initval_s;
typedef struct initval_s initval_t;
struct seg_s;
typedef struct seg_s seg_t;


seg_t *seg_alloc_static(stgctx_t ctx, textpos_t defpos, psect_t *psect);
seg_t *seg_alloc_stack(stgctx_t ctx, textpos_t defpos, int stackonly);
seg_t *seg_alloc_literal(stgctx_t ctx, textpos_t defpos, unsigned long value);
void seg_free(stgctx_t ctx, seg_t *seg);
int seg_initval_set(stgctx_t ctx, seg_t *seg, initval_t *ivlist);
void seg_static_psect_set(stgctx_t ctx, seg_t *seg, psect_t *psect);
psect_t *seg_static_psect(stgctx_t ctx, seg_t *seg);
frame_t *seg_stack_frame(stgctx_t ctx, seg_t *seg);
unsigned long seg_size(stgctx_t ctx, seg_t *seg);
void seg_size_set(stgctx_t ctx, seg_t *seg, unsigned long size);
unsigned int seg_alignment(stgctx_t ctx, seg_t *seg);
void seg_alignment_set(stgctx_t ctx, seg_t *seg, unsigned int alignment);
unsigned int seg_flags(stgctx_t ctx, seg_t *seg);
void seg_flags_set(stgctx_t ctx, seg_t *seg, unsigned int flags);
int seg_commit(stgctx_t ctx, seg_t *seg);
int seg_has_storage(stgctx_t ctx, seg_t *seg);

frame_t *frame_begin(stgctx_t ctx, textpos_t defpos, frame_t *parent);
void frame_end(stgctx_t ctx, frame_t *fr);

psect_t *psect_create(stgctx_t ctx, strdesc_t *name, textpos_t defpos, unsigned int attr);

initval_t *initval_scalar_add(stgctx_t ctx, initval_t *head, unsigned int reps,
                              long val, unsigned int width, int signext);
initval_t *initval_scalar_prepend(stgctx_t ctx, initval_t *head, unsigned int reps,
                                  long val, unsigned int width, int signext);
initval_t *initval_string_add(stgctx_t ctx, initval_t *head, unsigned int reps,
                              strdesc_t *str);
initval_t *initval_ivlist_add(stgctx_t ctx, initval_t *head, unsigned int reps,
                              initval_t *sublist);
void initval_freelist(stgctx_t ctx, initval_t *iv);
unsigned long initval_size(stgctx_t ctx, initval_t *ivlist);

stgctx_t storage_init(machinedef_t *mach);
void storage_finish(stgctx_t ctx);

strdesc_t *seg_dumpinfo(seg_t *seg);

#undef SIU
#endif
