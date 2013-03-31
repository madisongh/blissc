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

struct libgen_ctx_s;
typedef struct libgen_ctx_s *libgen_ctx_t;

libgen_ctx_t libgen_init(const char *libname, int lnlen);
int libgen_parse(libgen_ctx_t lgctx, expr_ctx_t ectx);
void libgen_finish(libgen_ctx_t lgctx);

#endif /* libgen_h__ */
