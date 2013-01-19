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

    return ctx;

} /* blissc_init */

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

    status = parser_fopen_main(ctx->pctx, fname, len, 0, 0, 0);
    if (status) status = declare_module(ctx->ectx);

    return status;
}

void
blissc_finish (blissc_driverctx_t ctx)
{
    if (ctx->ectx != 0) expr_finish(ctx->ectx);
//    if (ctx->pctx != 0) parser_finish(ctx->pctx);
    if (ctx->fioctx != 0) fileio_finish(ctx->fioctx);
    if (ctx->logctx != 0) logging_finish(ctx->logctx);
    if (ctx->strctx != 0) strings_finish(ctx->strctx);
    free(ctx);

} /* blissc_finish */

