//
//  storage.c
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "machinedef.h"
#include "storage.h"

struct stgctx_s {
    machinedef_t    *mach;
    psect_t         *psects;
    block_t         *blocks;
    block_t         *freeblocks;
    seg_t           *freesegs;
    initval_t       *freeivs;
};
#define BLOCK_ALLOCOUNT 128
#define SEG_ALLOCOUNT   128
#define IV_ALLOCOUNT    128

stgctx_t
storage_init (machinedef_t *mach)
{
    struct stgctx_s *ctx = malloc(sizeof(struct stgctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct stgctx_s));
        ctx->mach = mach;
    }
    return ctx;

} /* storage_init */

void
storage_finish (stgctx_t ctx)
{
    return; // XXX to be filled in later

} /* storage_finish */

psect_t *
psect_alloc (stgctx_t ctx, strdesc_t *name)
{
    psect_t *psect = malloc(sizeof(psect_t));

    if (psect != 0) {
        memset(psect, 0, sizeof(psect_t));
        psect->name = string_copy(0, name);
        psect->next = ctx->psects;
        ctx->psects = psect;
    }
    return psect;
} /* psect_alloc */

void
psect_free (stgctx_t ctx, psect_t *psect)
{
    psect_t *p, *lp;
    for (p = ctx->psects, lp = 0; p != 0; lp = p, p = p->next) {
        if (p == psect) break;
    }
    if (p != psect) {
        return;
    }
    if (lp == 0) {
        ctx->psects = psect->next;
    } else {
        lp->next = psect->next;
    }
    string_free(psect->name);
    memset(psect, 0xf0, sizeof(psect_t));
    free(psect);
    // XXX - free the blocks and segments, too

} /* psect_free */

block_t *
block_alloc (stgctx_t ctx, block_t *parent)
{
    block_t *blk;

    if (ctx->freeblocks == 0) {
        int i;
        ctx->freeblocks = malloc(sizeof(block_t)*BLOCK_ALLOCOUNT);
        for (i = 0, blk = ctx->freeblocks; i < BLOCK_ALLOCOUNT-1; i++, blk++) {
            blk->parent = blk + 1;
        }
        blk->parent = 0;
    }
    blk = ctx->freeblocks;
    ctx->freeblocks = blk->parent;
    memset(blk, 0, sizeof(block_t));
    blk->parent = parent;
    return blk;
} /* block_alloc */

void
block_free (stgctx_t ctx, block_t *blk)
{
    memset(blk, 0xe9, sizeof(block_t));
    blk->parent = ctx->freeblocks;
    ctx->freeblocks = blk;
} /* block_free */

seg_t *
seg_alloc (stgctx_t ctx)
{
    seg_t *seg;

    if (ctx->freesegs == 0) {
        int i;
        ctx->freesegs = malloc(sizeof(seg_t)*SEG_ALLOCOUNT);
        for (i = 0, seg = ctx->freesegs; i < SEG_ALLOCOUNT-1; i++, seg++) {
            seg->next = seg + 1;
        }
        seg->next = 0;
    }
    seg = ctx->freesegs;
    ctx->freesegs = seg->next;
    memset(seg, 0, sizeof(seg_t));
    return seg;
} /* seg_alloc */

void
seg_free (stgctx_t ctx, seg_t *seg)
{
    initval_freelist(ctx, seg->initializer);
    memset(seg, 0x7a, sizeof(seg_t));
    seg->next = ctx->freesegs;
    ctx->freesegs = seg;

} /* seg_free */

initval_t *
initval_alloc (stgctx_t ctx)
{
    initval_t *iv;

    if (ctx->freeivs == 0) {
        int i;
        ctx->freeivs = malloc(sizeof(initval_t)*IV_ALLOCOUNT);
        for (i = 0, iv = ctx->freeivs; i < IV_ALLOCOUNT-1; i++, iv++) {
            iv->next = iv + 1;
        }
        iv->next = 0;
    }
    iv = ctx->freeivs;
    ctx->freeivs = iv->next;
    memset(iv, 0, sizeof(initval_t));
    return iv;

} /* initval_alloc */

void
initval_freelist (stgctx_t ctx, initval_t *iv)
{
    initval_t *nextiv;

    while (iv != 0) {
        nextiv = iv->next;
        iv->next = ctx->freeivs;
        ctx->freeivs = iv;
        iv = nextiv;
    }

} /* initval_freelist */