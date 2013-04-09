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
#include <stdio.h>
#include <string.h>
#include "blissc/libgen.h"
#include "blissc/declarations.h"
#include "blissc/expression.h"
#include "blissc/machinedef.h"
#include "blissc/support/fileio.h"

struct libgen_ctx_s {
    fioctx_t fioctx;
    filectx_t file;
    char *libname;
    libgen_compilerinfo_t compilerinfo;
};

#define LIB_HEADER_VERSION  1

static int
write_header (libgen_ctx_t lgctx, const char *target_triple)
{
    unsigned char hdrbuf[8];
    int htlen = (int) strlen(lgctx->compilerinfo.host_triple);
    int ttlen = (int) strlen(target_triple);

    if (htlen >= 255) htlen = 255;
    if (ttlen >= 255) ttlen = 255;

    hdrbuf[0] = 'B';
    hdrbuf[1] = 'L';
    hdrbuf[2] = 'B';
    hdrbuf[3] = LIB_HEADER_VERSION;
    hdrbuf[4] = (unsigned char) lgctx->compilerinfo.ver_major;
    hdrbuf[5] = (unsigned char) lgctx->compilerinfo.ver_minor;
    hdrbuf[6] = (unsigned char) htlen;
    hdrbuf[7] = (unsigned char) ttlen;
    if (file_writebuf(lgctx->file, hdrbuf, sizeof(hdrbuf)) < 0) {
        return -1;
    }
    if (file_writebuf(lgctx->file, lgctx->compilerinfo.host_triple, htlen) < 0) {
        return -1;
    }
    return file_writebuf(lgctx->file, target_triple, ttlen);

} /* write_header */

/*
 * libgen_init
 *
 * Initialize library generation, setting the name of the
 * output file.
 */
libgen_ctx_t
libgen_init (fioctx_t fioctx, const char *libname, int lnlen,
             libgen_compilerinfo_t *info)
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
    ctx->fioctx = fioctx;
    memcpy(&ctx->compilerinfo, info, sizeof(libgen_compilerinfo_t));
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
    scopectx_t scope = parser_scope_get(expr_parse_ctx(ctx));
//    void *walkctx = 0;
//    name_t *np;

    expr_libgen_set(ctx, 1);
    parse_libgen_declarations(ctx);

    lgctx->file = file_open_output(lgctx->fioctx, lgctx->libname, -1);
    if (lgctx->file == 0) {
        return 0;
    }
    if (write_header(lgctx, machine_triple(expr_machinedef(ctx))) < 0) {
        return 0;
    }
    scope_serialize(scope, lgctx->file);
#if 0
    for (np = scope_nextname(scope, &walkctx); np != 0;
         np = scope_nextname(scope, &walkctx)) {
        if ((name_flags(np) & (NAME_M_DECLARED|NAME_M_FROMLIB)) == NAME_M_DECLARED) {
            strdesc_t *n = name_string(np);
            printf("Gen: %-*.*s\n", n->len, n->len, n->ptr);
            // XXX process this name
        }
    }
#endif
    file_close(lgctx->file);

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