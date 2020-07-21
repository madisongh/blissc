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

#include "machinedef.h"
#include "support/logging.h"
#include "support/fileio.h"
#include "support/utils.h"

struct libgen_ctx_s;
typedef struct libgen_ctx_s *libgen_ctx_t;

int lib_parse_header(logctx_t logctx, textpos_t curpos,
                     filectx_t fh, compilerinfo_t *me, machinedef_t *mach);
libgen_ctx_t libgen_init(fioctx_t fioctx, const char *libname, size_t lnlen,
                         compilerinfo_t *compilerinfo);
int libgen_parse(libgen_ctx_t lgctx, void *exprctx);
void libgen_finish(libgen_ctx_t lgctx);

#endif /* libgen_h__ */
