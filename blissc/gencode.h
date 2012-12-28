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

#include "expression.h"
#include "storage.h"

struct gencodectx_s;
typedef struct gencodectx_s *gencodectx_t;

gencodectx_t gencode_init(expr_ctx_t ectx, stgctx_t stg);
void gencode_finish(gencodectx_t gctx);


#endif /* gencode_h__ */
