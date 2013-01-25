/*
 *++
 *	File:			driver.c
 *
 *	Abstract:		Compiler "driver" interface
 *
 *  Module description:
 *		This module implements the upper edge of the compiler,
 *      providing a callable interface to specify compiler options
 *      and exposing these options to other parts of the compiler.
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		18-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */
#include "blissc/driver.h"
#include "blissc/support/strings.h"
#include "blissc/support/logging.h"
#include "blissc/support/fileio.h"
#include "blissc/machinedef.h"
#include "blissc/nametable.h"
#include "blissc/parser.h"
#include "blissc/expression.h"
#include "blissc/declarations.h"
#include <stdlib.h>

struct blissc_driverctx_s {
    strctx_t        strctx;
    logctx_t        logctx;
    fioctx_t        fioctx;
    parse_ctx_t     pctx;
    expr_ctx_t      ectx;
    machinedef_t    *mach;
    scopectx_t      kwdscope;
    unsigned int    variant;
    unsigned int    listflags;
    machine_output_t outtype;
    char            *outfn;
    unsigned int    outfnlen;
    int             free_outfn;
    char            *listfn;
    unsigned int    listfnlen;
    int             free_listfn;
};

blissc_driverctx_t
blissc_init (jmp_buf retenv)
{
    blissc_driverctx_t ctx;

    ctx = malloc(sizeof(struct blissc_driverctx_s));
    if (ctx == 0) return 0;
    memset(ctx, 0, sizeof(struct blissc_driverctx_s));
    ctx->strctx = strings_init();
    ctx->logctx = logging_init(retenv);
    ctx->fioctx = fileio_init(ctx->logctx);
    ctx->outtype = MACH_K_OUTPUT_OBJ;

    return ctx;

} /* blissc_init */

int
blissc_output_set (blissc_driverctx_t ctx, bliss_output_t outtype,
                   const char *fname, int fnlen)
{
    ctx->outtype = (outtype == BLISS_K_OUTPUT_ASSEMBLY ? MACH_K_OUTPUT_ASM : MACH_K_OUTPUT_OBJ);
    if (fname != 0) {
        ctx->outfn = file_canonicalname(ctx->fioctx, fname, fnlen, &ctx->outfnlen);
        if (ctx->outfn == 0) return 0;
    }
    return 1;
} /* blissc_output_set */

int
blissc_listopt_set (blissc_driverctx_t ctx, unsigned int flags,
                    const char *fname, int fnlen)
{
    ctx->listflags = flags;
    if (fname != 0) {
        ctx->listfn = file_canonicalname(ctx->fioctx, fname, fnlen, &ctx->listfnlen);
        if (ctx->listfn == 0) return 0;
    }
    return 1;

} /* blissc_listopt_set */

int blissc_variant_set (blissc_driverctx_t ctx, unsigned int val)
{
    ctx->variant = val;
    return 1;
}

int
blissc_target_set (blissc_driverctx_t ctx, const char *machspec)
{
    ctx->mach = machine_init(machspec);
    if (ctx->mach == 0) return 0;
    ctx->pctx = parser_init(ctx->strctx, 0, ctx->mach, &ctx->kwdscope, ctx->logctx, ctx->fioctx);
    ctx->ectx = expr_init(ctx->strctx, ctx->pctx, ctx->kwdscope);
    return 1;
}

int
blissc_compile (blissc_driverctx_t ctx, const char *fname, int fnlen)
{
    int status;
    size_t len = (fnlen < 0 ? strlen(fname) : fnlen);
    fio_pathparts_t fnparts;

    if (ctx->variant != 0) parser_variant_set(ctx->pctx, ctx->variant);
    if (ctx->outfn == 0) {
        if (!file_splitname(ctx->fioctx, fname, fnlen, 1, &fnparts)) {
            return 0;
        }
        if (ctx->outtype == BLISS_K_OUTPUT_ASSEMBLY) {
            fnparts.path_suffix = ".s";
        } else {
            fnparts.path_suffix = ".o";
        }
        fnparts.path_suffixlen = 2;
        if (!file_combinename(ctx->fioctx, &fnparts)) {
            return 0;
        }
        ctx->outfn = fnparts.path_fullname;
        ctx->outfnlen = (unsigned int) fnparts.path_fullnamelen;
        ctx->free_outfn = 1;
        machine_output_set(ctx->mach, ctx->outtype, ctx->outfn, ctx->outfnlen);
    }

    if (ctx->listflags == 0) {
        ctx->listfn = 0;
        ctx->listfnlen = 0;
    } else if (ctx->listfn == 0) {
        if (!file_splitname(ctx->fioctx, ctx->outfn, ctx->outfnlen, 1, &fnparts)) {
            return 0;
        }
        fnparts.path_suffix = ".lis";
        fnparts.path_suffixlen = 4;
        if (!file_combinename(ctx->fioctx, &fnparts)) {
            return 0;
        }
        ctx->listfn = fnparts.path_fullname;
        ctx->listfnlen = (unsigned int) fnparts.path_fullnamelen;
        ctx->free_listfn = 1;
    }
    status = parser_fopen_main(ctx->pctx, fname, len,
                               ctx->listflags, ctx->listfn, ctx->listfnlen);
    if (status) status = declare_module(ctx->ectx);

    return status;
}

void
blissc_finish (blissc_driverctx_t ctx)
{
    if (ctx->ectx != 0) expr_finish(ctx->ectx); // which also calls parser_finish()
    if (ctx->fioctx != 0) fileio_finish(ctx->fioctx);
    if (ctx->logctx != 0) logging_finish(ctx->logctx);
    if (ctx->strctx != 0) strings_finish(ctx->strctx);
    if (ctx->free_listfn) free(ctx->listfn);
    if (ctx->free_outfn) free(ctx->outfn);
    free(ctx);

} /* blissc_finish */

