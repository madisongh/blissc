/*
 *++
 * driver.c - Compiler "driver" interface
 *
 * This module implements the upper edge of the compiler,
 * providing a callable interface to specify compiler options
 * and exposing these options to other parts of the compiler.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "blissc/driver.h"
#include "blissc/support/strings.h"
#include "blissc/support/logging.h"
#include "blissc/support/fileio.h"
#include "blissc/machinedef.h"
#include "blissc/nametable.h"
#include "blissc/gencode.h"
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
    int             optlevel;
    int             dumpir;
    char            *irfn;
    unsigned int    irfnlen;
    int             free_irfn;
};

/*
 * blissc_init
 *
 * Initializes the driver, setting up the context block.
 * This should be first routine called by a driver program.
 */
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
    ctx->optlevel = -1; // unset

    return ctx;

} /* blissc_init */

/*
 * blissc_output_set
 *
 * Sets the output format and, optionally, the output
 * file name.
 */
int
blissc_output_set (blissc_driverctx_t ctx, bliss_output_t outtype,
                   const char *fname, int fnlen)
{
    ctx->outtype = (outtype == BLISS_K_OUTPUT_ASSEMBLY ? MACH_K_OUTPUT_ASM : MACH_K_OUTPUT_OBJ);
    if (fname != 0) {
        if (fnlen < 0) fnlen = (int) strlen(fname);
        ctx->outfn = malloc(fnlen+1);
        if (ctx->outfn == 0) return 0;
        memcpy(ctx->outfn, fname, fnlen);
        ctx->outfn[fnlen] = '\0';
        ctx->outfnlen = fnlen;
        ctx->free_outfn = 1;
    }
    return 1;

} /* blissc_output_set */

/*
 * blissc_listopt_set
 *
 * Sets the listing options and, optionally,
 * the listing file name.
 */
int
blissc_listopt_set (blissc_driverctx_t ctx, unsigned int flags,
                    const char *fname, int fnlen)
{
    ctx->listflags = flags;
    if (fname != 0) {
        if (fnlen < 0) fnlen = (int) strlen(fname);
        ctx->listfn = malloc(fnlen);
        if (ctx->listfn == 0) return 0;
        memcpy(ctx->listfn, fname, fnlen);
        ctx->listfn[fnlen] = '\0';
        ctx->listfnlen = fnlen;
        ctx->free_listfn = 1;
    }
    return 1;

} /* blissc_listopt_set */

/*
 * Sets the option to dump the IR output to a file
 * and, optionally, the filename for that output.
 */
int
blissc_dumpir_set (blissc_driverctx_t ctx, int val,
                    const char *fname, int fnlen)
{
    ctx->dumpir = val;
    if (fname != 0) {
        if (fnlen < 0) fnlen = (int) strlen(fname);
        ctx->irfn = malloc(fnlen);
        if (ctx->irfn == 0) return 0;
        memcpy(ctx->irfn, fname, fnlen);
        ctx->irfn[fnlen] = '\0';
        ctx->irfnlen = fnlen;
        ctx->free_irfn = 1;
    }
    return 1;

} /* blissc_dumpir_set */

/*
 * blissc_variant_set
 *
 * Sets the value for the %VARIANT lexical
 * function.
 */
int blissc_variant_set (blissc_driverctx_t ctx, unsigned int val)
{
    ctx->variant = val;
    return 1;

} /* blissc_variant_set */

/*
 * blissc_target_set
 *
 * Sets the target tuple string for the compiler.
 */
int
blissc_target_set (blissc_driverctx_t ctx, const char *machspec)
{
    ctx->mach = machine_init(machspec);
    if (ctx->mach == 0) return 0;
    ctx->pctx = parser_init(ctx->strctx, 0, ctx->mach, &ctx->kwdscope, ctx->logctx, ctx->fioctx);
    ctx->ectx = expr_init(ctx->strctx, ctx->pctx, ctx->kwdscope);
    return 1;

} /* blissc_target_set */

/*
 * blissc_optlevel_set
 *
 * Sets the optimization level.
 */
int
blissc_optlevel_set (blissc_driverctx_t ctx, unsigned int val)
{
    ctx->optlevel = val;
    return 1;

} /* blissc_optlevel_set */

/*
 * blissc_compile
 *
 * Begins a compilation.  This should be called only
 * after all of the desired compilation options have
 * been set -- they are processed here (passing the
 * settings down to the compiler modules) before
 * compilation begins.  The source file name is required.
 */
int
blissc_compile (blissc_driverctx_t ctx, const char *fname, int fnlen)
{
    int status;
    size_t len = (fnlen < 0 ? strlen(fname) : fnlen);
    fio_pathparts_t srcparts, objparts, lstparts;

    if (!file_splitname(ctx->fioctx, fname, fnlen, 1, &srcparts)) {
        return 0;
    }
    if (ctx->variant != 0) parser_variant_set(ctx->pctx, ctx->variant);
    if (ctx->outfn == 0) {
        file_splitname(ctx->fioctx, srcparts.path_fullname,
                       (int) srcparts.path_fullnamelen, 0, &objparts);
        if (ctx->outtype == MACH_K_OUTPUT_ASM) {
            objparts.path_suffix = ".s";
        } else {
            objparts.path_suffix = ".o";
        }
        objparts.path_suffixlen = 2;
        objparts.path_dirnamelen = 0;
        if (!file_combinename(ctx->fioctx, &objparts)) {
            return 0;
        }
        free(ctx->outfn);
        ctx->outfn = objparts.path_fullname;
        ctx->outfnlen = (unsigned int) objparts.path_fullnamelen;
        ctx->free_outfn = 1;
    }
    free(srcparts.path_fullname);
    machine_output_set(ctx->mach, ctx->outtype, ctx->outfn, ctx->outfnlen);

    if (ctx->listflags == 0) {
        if (ctx->listfn != 0 && ctx->free_listfn) free(ctx->listfn);
        ctx->listfn = 0;
        ctx->listfnlen = 0;
    } else {
        if (ctx->listfn == 0) {
            file_splitname(ctx->fioctx, ctx->outfn, ctx->outfnlen, 0, &lstparts);
            lstparts.path_suffix = ".lis";
            lstparts.path_suffixlen = 4;
        } else {
            if (!file_splitname(ctx->fioctx, ctx->listfn, -1, 0, &lstparts)) {
                return 0;
            }
            if (lstparts.path_dirnamelen == 0) {
                file_splitname(ctx->fioctx, ctx->outfn, ctx->outfnlen, 0, &objparts);
                lstparts.path_dirnamelen = objparts.path_dirnamelen;
                lstparts.path_dirname = objparts.path_dirname;
                free(objparts.path_fullname);
            }
        }
        if (!file_combinename(ctx->fioctx, &lstparts)) {
            return 0;
        }
        if (ctx->listfn != 0 && ctx->free_listfn) free(ctx->listfn);
        ctx->listfn = lstparts.path_fullname;
        ctx->listfnlen = (unsigned int) lstparts.path_fullnamelen;
        ctx->free_listfn = 1;
    }

    if (ctx->dumpir) {
        if (ctx->irfn == 0) {
            file_splitname(ctx->fioctx, ctx->outfn, ctx->outfnlen, 0, &lstparts);
            lstparts.path_suffix = ".ll";
            lstparts.path_suffixlen = 3;
        } else {
            if (!file_splitname(ctx->fioctx, ctx->irfn, ctx->irfnlen, 0, &lstparts)) {
                return 0;
            }
        }
        if (!file_combinename(ctx->fioctx, &lstparts)) {
            return 0;
        }
        if (ctx->irfn != 0 && ctx->free_irfn) free(ctx->irfn);
        ctx->irfn = lstparts.path_fullname;
        ctx->irfnlen = (unsigned int) lstparts.path_fullnamelen;
        ctx->free_irfn = 1;
    }
    machine_dumpir_set(ctx->mach, ctx->irfn, ctx->irfnlen);

    if (ctx->optlevel >= 0) {
        gencode_optlevel_set(expr_gencodectx(ctx->ectx), ctx->optlevel);
    }
    status = parser_fopen_main(ctx->pctx, fname, len,
                               ctx->listflags, ctx->listfn, ctx->listfnlen);
    if (status) status = declare_module(ctx->ectx);

    return status;

} /* blissc_compile */

/*
 * blissc_finish
 *
 * Cleans up and frees the compilation context.
 */
void
blissc_finish (blissc_driverctx_t ctx)
{
    if (ctx->ectx != 0) expr_finish(ctx->ectx); // which also calls parser_finish()
    if (ctx->mach) machine_finish(ctx->mach);
    if (ctx->fioctx != 0) fileio_finish(ctx->fioctx);
    if (ctx->logctx != 0) logging_finish(ctx->logctx);
    if (ctx->strctx != 0) strings_finish(ctx->strctx);
    if (ctx->free_listfn) free(ctx->listfn);
    if (ctx->free_outfn) free(ctx->outfn);
    if (ctx->free_irfn) free(ctx->irfn);
    free(ctx);

} /* blissc_finish */
