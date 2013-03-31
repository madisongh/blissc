/*
 *++
 * libgen.c - Library generation
 *
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdlib.h>
#include <string.h>
#include "blissc/libgen.h"
#include "blissc/declarations.h"
#include "blissc/expression.h"

struct libgen_ctx_s {
    char *libname;
};

/*
 * libgen_init
 *
 * Initialize library generation, setting the name of the
 * output file.
 */
libgen_ctx_t
libgen_init (const char *libname, int lnlen)
{
    libgen_ctx_t ctx;

    if (lnlen < 0) {
        lnlen = (int)strlen(libname);
    }
    ctx = malloc(sizeof(struct libgen_ctx_s)+(lnlen+1));
    if (ctx == 0) {
        return 0;
    }
    memset(ctx, 0, sizeof(struct libgen_ctx_s));
    ctx->libname = ((char *)ctx) + sizeof(struct libgen_ctx_s);
    memcpy(ctx->libname, libname, lnlen);
    ctx->libname[lnlen] = '\0';

    return ctx;

} /* libgen_init */

/*
 * libgen_parse
 *
 * Parse declarations and generate a library.
 */
int
libgen_parse (libgen_ctx_t lgctx, expr_ctx_t ctx)
{
    expr_libgen_set(ctx, 1);
    parse_libgen_declarations(ctx);
    // Walk the name table and dump out
    // all declared symbols
    return 1;

} /* libgen_parse */

/*
 * libgen_finish
 *
 * Module cleanup.
 */
void
libgen_finish (libgen_ctx_t lgctx)
{
    if (lgctx != 0) free(lgctx);

} /* libgen_finish */