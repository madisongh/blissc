#ifndef execfuncs_h__
#define execfuncs_h__
/*
 *++
 *	File:			execfuncs.h
 *
 *	Abstract:		Definitions for executable functions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "expression.h"
#include "nametable.h"
#include "support/strings.h"

typedef expr_node_t *(*execfunchandler_fn)(expr_ctx_t ctx,
                        void *fctx, name_t *fnp,
                        exprseq_t *arglist, textpos_t curpos);

#define FUNC_M_VARARGS  (1<<0)
#define FUNC_M_NOVALUE  (1<<1)
struct funcdef_s {
    execfunchandler_fn  handler;
    void                *fnctx;
    unsigned int        numargs; // min # of args when VARARGS
    unsigned int        flags;
    unsigned short      namelen;
    char                name[NAME_SIZE];
};

typedef struct funcdef_s funcdef_t;

/*
 * FUNCDEF(name, handler, contextptr, argcount, flags)
 */
#define FUNCDEF(n_, h_, c_, a_, f_) { (h_), (c_), (a_), (f_), sizeof(n_)-1, (n_) }

void execfunc_init(expr_ctx_t ctx, scopectx_t scope);
name_t *execfunc_define(scopectx_t ctx, funcdef_t *funcdef, textpos_t pos);

#endif /* execfuncs_h__ */
