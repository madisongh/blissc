//
//  storage.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_storage_h
#define blissc_storage_h

#include "machinedef.h"
#include "strings.h"
#include <stdint.h>

typedef struct stgctx_s *stgctx_t;

typedef enum {
    SCLASS_OWN,
    SCLASS_GLOBAL,
    SCLASS_PLIT,
    SCLASS_CODE
} storageclass_t;

typedef enum {
    ADDRMODE_ABSOLUTE,
    ADDRMODE_RELATIVE
} addrmode_t;

struct seg_s;

struct psect_s {
    struct psect_s  *next;
    strdesc_t       *name;
    storageclass_t   class;
    unsigned int     alignment;
    addrmode_t       addrmode;
    intptr_t         baseaddr;
    intptr_t         size;
    void            *machattr;
    struct seg_s    *segchain;
};
typedef struct psect_s psect_t;


struct block_s {
    struct psect_s  *psect;
    struct block_s  *parent;
    struct seg_s    *segchain;
    intptr_t         framebase;
};
typedef struct block_s block_t;

struct initval_s {
    struct initval_s *next;
    unsigned int    flags;
#define INITVAL_M_STRING (1<<0)
    unsigned int    repcount; // for INITIAL
    unsigned int    offset, size, extend; // for PRESET
                    // size is also used for PLIT allocation units
    intptr_t        value; // pointer for strings, value otherwise
};
typedef struct initval_s initval_t;

#undef DOSEGTYPE
#define DOSEGTYPES \
    DOSEGTYPE(LITERAL) DOSEGTYPE(LOCAL) \
    DOSEGTYPE(REGISTER) DOSEGTYPE(STACKLOCAL) \
    DOSEGTYPE(OTHER)
#define DOSEGTYPE(t_) SEGTYPE_##t_,
typedef enum {
    DOSEGTYPES
} segtype_t;
#undef DOSEGTYPE

#define SEG_M_VOLATILE (1<<1)
#define SEG_M_EXTERNAL (1<<3)
#define SEG_M_GLOBAL   (1<<4)
#define SEG_M_HASVAL   (1<<5)
#define SEG_M_REF      (1<<6)
#define SEG_M_ALIAS    (1<<7)
#define SEG_M_SIGNED   (1<<8)
struct seg_s {
    struct seg_s    *next;
    void            *container; // block when stack-allocated; psect otherwise
    intptr_t         offset; // for literals, holds the value
    segtype_t        type;
    unsigned long    size; // in units (bits for literals)
    initval_t       *initializer;
    unsigned long    flags;
    void             *machattr;
};
typedef struct seg_s seg_t;

static inline __unused block_t *seg_block (seg_t *seg) { return seg->container; }
static inline __unused psect_t *seg_psect (seg_t *seg) { return seg->container; }

seg_t *seg_alloc(stgctx_t ctx);
void seg_free(stgctx_t ctx, seg_t *seg);
block_t *block_alloc(stgctx_t ctx);
void block_free(stgctx_t ctx, block_t *block);
block_t *module_block(stgctx_t ctx);
psect_t *psect_alloc(stgctx_t ctx, strdesc_t *name);
void psect_free(stgctx_t ctx, psect_t *psect);
initval_t *initval_alloc(stgctx_t ctx);
void initval_freelist(stgctx_t ctx, initval_t *iv);

stgctx_t storage_init(machinedef_t *mach);
void storage_finish(stgctx_t ctx);

strdesc_t *seg_dumpinfo(seg_t *seg);
#endif
