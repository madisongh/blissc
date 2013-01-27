#ifndef gencode_h__
#define gencode_h__
/*
 *++
 *	File:			gencode.h
 *
 *	Abstract:		Generic interface for gencode modules.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */

#include "nametable.h"
#include "symbols.h"
#include "support/logging.h"
#include "machinedef.h"

struct gencodectx_s;
typedef struct gencodectx_s *gencodectx_t;

gencodectx_t gencode_init(void *ectx, logctx_t logctx, machinedef_t *mach, symctx_t symctx);
void gencode_postinit(gencodectx_t gctx);
void gencode_finish(gencodectx_t gctx);
int gencode_module_begin(gencodectx_t gctx, name_t *np);
int gencode_module_end(gencodectx_t gctx, name_t *np);
int gencode_routine_begin(gencodectx_t gctx, name_t *np);
int gencode_routine_end(gencodectx_t gctx, name_t *np);
void gencode_optlevel_set(gencodectx_t gctx, unsigned int level);

#endif /* gencode_h__ */
