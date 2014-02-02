/*
 *++
 * blissir.c - main BLISS IR module
 *
 *
 * Copyright © 2014  Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "blissir_private.h"
#include <stdlib.h>


blissir_ctx_t
blissir_init (machinedef_t *mach) {
    blissir_ctx_t ctx;
    ctx = malloc(sizeof(struct blissir_ctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct blissir_ctx_s));
    }
    return ctx;
    
} /* blissir_init */