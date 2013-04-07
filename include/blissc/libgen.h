#ifndef libgen_h__
#define libgen_h__
/*
 *++
 * libgen.h - LIBRARY generation definitions.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license.  See LICENSE.TXT for details.
 *--
 */

#include "expression.h"
#include "support/fileio.h"

struct libgen_ctx_s;
typedef struct libgen_ctx_s *libgen_ctx_t;

struct libgen_compilerinfo_s {
    unsigned int    ver_major, ver_minor;
    const char     *host_triple;
};
typedef struct libgen_compilerinfo_s libgen_compilerinfo_t;

libgen_ctx_t libgen_init(fioctx_t fioctx, const char *libname, int lnlen,
                         libgen_compilerinfo_t *compilerinfo);
int libgen_parse(libgen_ctx_t lgctx, expr_ctx_t ectx);
void libgen_finish(libgen_ctx_t lgctx);

#endif /* libgen_h__ */
