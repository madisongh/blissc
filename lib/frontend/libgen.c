/*
 *++
 * libgen.c - Library generation
 *
 * This module handles library file creation and
 * the reading of a library file's header.  Serialization
 * and deserialization of declared names are handled in
 * in the nametables module.
 *
 * Section 16.6 of the LRM describes BLISS libraries, including
 * the types of declarations that may be present in a library
 * source file.
 *
 * XXX
 * In the current implementation, the compiler version, host
 * triple, and target triple must match exactly between the
 * compiler generating a library and the one reading it.
 * Some versioning and backward compatibility mechanism would
 * be nice to have, as would allowing libraries generated on
 * one host to be readable by another (for the same target).
 * XXX
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
#include "blissc/support/logging.h"
#include "blissc/support/fileio.h"
#include "blissc/support/statcodes.h"
#include "blissc/support/utils.h"

struct libgen_ctx_s {
    fioctx_t fioctx;
    filectx_t file;
    char *libname;
    compilerinfo_t compilerinfo;
};

#define LIB_HEADER_VERSION  1

/*
 * write_header
 *
 * Writes a library header.  The header is written such that
 * it can be read on a different type of host (e.g., big vs.
 * little endian).
 */
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
 * lib_parse_header
 *
 * Check that the input stream is a valid library file by
 * checking it against the version, host triple, and target triple.
 */
int
lib_parse_header (logctx_t logctx, textpos_t curpos,
                  filectx_t fh, compilerinfo_t *me, machinedef_t *mach)
{
    const char *target = machine_triple(mach);
    unsigned char hdrbuf[8];
    char buf[256];
    size_t len;

    if (file_readbuf(fh, hdrbuf, sizeof(hdrbuf), &len) <= 0 ||
        len != sizeof(hdrbuf)) {
        return 0;
    }
    if (hdrbuf[0] != 'B' || hdrbuf[1] != 'L' || hdrbuf[2] != 'B'
        || hdrbuf[3] != LIB_HEADER_VERSION) {
        log_signal(logctx, curpos, STC__INVLIBHDR, file_getname(fh));
        return 0;
    }
    if (hdrbuf[4] != me->ver_major || hdrbuf[5] != me->ver_minor) {
        log_signal(logctx, curpos, STC__LIBVERMISM, file_getname(fh));
        return 0;
    }
    if (hdrbuf[6] != strlen(me->host_triple)) {
        log_signal(logctx, curpos, STC__LIBHSTMISM, file_getname(fh));
        return 0;
    }
    if (hdrbuf[7] != strlen(target)) {
        log_signal(logctx, curpos, STC__LIBTRGMISM, file_getname(fh));
        return 0;
    }
    if (file_readbuf(fh, buf, hdrbuf[6], &len) <= 0 ||
        len != hdrbuf[6]) {
        log_signal(logctx, curpos, STC__LIBRDERR, file_getname(fh));
        return 0;
    }
    if (memcmp(buf, me->host_triple, len) != 0) {
        log_signal(logctx, curpos, STC__LIBHSTMISM, file_getname(fh));
        return 0;
    }
    if (file_readbuf(fh, buf, hdrbuf[7], &len) <= 0 ||
        len != hdrbuf[7]) {
        log_signal(logctx, curpos, STC__LIBRDERR, file_getname(fh));
        return 0;
    }
    if (memcmp(buf, target, len) != 0) {
        log_signal(logctx, curpos, STC__LIBTRGMISM, file_getname(fh));
        return 0;
    }
    return 1;

} /* lib_parse_header */

/*
 * libgen_init
 *
 * Initialize library generation, setting the name of the
 * output file.
 */
libgen_ctx_t
libgen_init (fioctx_t fioctx, const char *libname, int lnlen,
             compilerinfo_t *info)
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
    memcpy(&ctx->compilerinfo, info, sizeof(compilerinfo_t));
    ctx->libname = ((char *)ctx) + sizeof(struct libgen_ctx_s);
    memcpy(ctx->libname, libname, lnlen);
    ctx->libname[lnlen] = '\0';

    return ctx;

} /* libgen_init */

/*
 * libgen_parse
 *
 * Parse declarations and call scope_serialize() to generate
 * the library.
 */
int
libgen_parse (libgen_ctx_t lgctx, void *vctx)
{
    expr_ctx_t ctx = vctx;
    scopectx_t scope = parser_scope_get(expr_parse_ctx(ctx));

    expr_libgen_set(ctx, 1);
    parse_libgen_declarations(ctx);

    lgctx->file = file_open_output(lgctx->fioctx, lgctx->libname, -1);
    if (lgctx->file == 0) {
        return 0;
    }
    if (write_header(lgctx, machine_triple(expr_machinedef(ctx))) < 0) {
        return 0;
    }
    scope_serialize(scope, lgctx->file, 0);
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