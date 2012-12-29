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
#include "storage.h"
#include "logging.h"
#include "machinedef.h"

struct gencodectx_s;
typedef struct gencodectx_s *gencodectx_t;

gencodectx_t gencode_init(logctx_t logctx, machinedef_t *mach, stgctx_t stg);
void gencode_finish(gencodectx_t gctx);
int gencode_module_begin(gencodectx_t gctx, void *exprctx, name_t *np);
int gencode_module_end(gencodectx_t gctx, name_t *np);
int gencode_datasym(gencodectx_t gctx, name_t *np);
int gencode_litsym(gencodectx_t gctx, name_t *np);
int gencode_rtnsym(gencodectx_t gctx, name_t *np);
int gencode_routine_begin(gencodectx_t gctx, name_t *np);
int gencode_routine_end(gencodectx_t gctx, name_t *np);

#endif /* gencode_h__ */
