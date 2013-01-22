#ifndef llvmgen_h__
#define llvmgen_h__
/*
 *++
 *	File:			llvmgen.h
 *
 *	Abstract:		Common definitions for LLVM code generation.
 *
 *	Author:			M. Madison
 *					Copyright © 2013, Matthew Madison
 *					All rights reserved.
 *--
 */

#include "blissc/gencode.h"
#include "blissc/execfuncs.h"
#include "blissc/expression.h"
#include "blissc/symbols.h"
#include "blissc/machinedef.h"
#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm_helper.h"
#include "llvm_machinectx.h"
#include <string.h>
#include <stdio.h>

#define LLVMGEN_K_PHIREFMAX     256

struct llvm_btrack_s {
    struct llvm_btrack_s    *next;
    LLVMBasicBlockRef        exitblock;
    struct {
        LLVMBasicBlockRef       source;
        LLVMValueRef            value;
    }                        phirefs[LLVMGEN_K_PHIREFMAX];
    unsigned int             branchcount;
    unsigned int             phirefcount;
};
typedef struct llvm_btrack_s llvm_btrack_t;

#define LLVMGEN_K_BT_FUNC   0
#define LLVMGEN_K_BT_BLK    1
#define LLVMGEN_K_BT_LOOP   2
#define LLVMGEN_K_BTCOUNT   3

struct llvm_rtntrack_s {
    struct llvm_rtntrack_s  *next;
    llvm_btrack_t           *btrack[LLVMGEN_K_BTCOUNT];
    LLVMBuilderRef           builder;
    LLVMValueRef             func;
    LLVMBasicBlockRef        bldpos;
    unsigned int             tmpidx;
    unsigned int             lblidx;
};
typedef struct llvm_rtntrack_s llvm_rtntrack_t;

struct gencodectx_s {
    machine_ctx_t       mctx;
    expr_ctx_t          ectx;
    symctx_t            symctx;
    machinedef_t        *mach;
    name_t              *modnp;
    llvm_btrack_t       *freebts;
    llvm_rtntrack_t     *freerts;
    llvm_rtntrack_t     *curfn;
    name_t              *extern_psect;

    LLVMContextRef      llvmctx;
    LLVMModuleRef       module;
    LLVMPassManagerRef  passmgr;

    unsigned int        globidx;

    char                tmpnambuf[NAME_SIZE];
};

typedef enum {
    LLVM_REG,
    LLVM_LOCAL,
    LLVM_GLOBAL
} llvm_stgclass_t;

typedef LLVMValueRef (*llvmgen_expgen_fn)(gencodectx_t, expr_node_t *, LLVMTypeRef);

#define siu static inline __attribute__((unused))
siu char *llvmgen_temp(gencodectx_t gctx) {
    snprintf(gctx->tmpnambuf, sizeof(gctx->tmpnambuf), "tmp.%u", gctx->curfn->tmpidx++);
    return gctx->tmpnambuf;
}
siu char *llvmgen_label(gencodectx_t gctx) {
    snprintf(gctx->tmpnambuf, sizeof(gctx->tmpnambuf), "label.%u", gctx->curfn->lblidx++);
    return gctx->tmpnambuf;
}
siu char *llvmgen_global(gencodectx_t gctx) {
    snprintf(gctx->tmpnambuf, sizeof(gctx->tmpnambuf), "$$global$$.%u", gctx->globidx++);
    return gctx->tmpnambuf;
}
siu void llvmgen_btrack_update_brcount(gencodectx_t gctx, llvm_btrack_t *bt) {
    bt->branchcount += 1;
}
#undef siu

LLVMBasicBlockRef llvmgen_exitblock_create(gencodectx_t gctx, char *label);
llvm_btrack_t *llvmgen_btrack_create(gencodectx_t gctx, LLVMBasicBlockRef exitpoint);
void llvmgen_btrack_update(gencodectx_t gctx, llvm_btrack_t *bt, LLVMValueRef val);
void llvmgen_btrack_update_phi(gencodectx_t gctx, llvm_btrack_t *bt,
                               LLVMBasicBlockRef pos, LLVMValueRef val);
void llvmgen_btrack_free(gencodectx_t gctx, llvm_btrack_t *bt);
LLVMValueRef llvmgen_btrack_finalize(gencodectx_t gctx, llvm_btrack_t *bt);
void llvmgen_symgen_init(gencodectx_t gctx);
void llvmgen_memcpy(gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef src, char *name);
void llvmgen_memset(gencodectx_t gctx, LLVMValueRef dest, LLVMValueRef val, char *name);
LLVMValueRef llvmgen_assignment(gencodectx_t gctx, expr_node_t *lhs, expr_node_t *rhs);
LLVMValueRef llvmgen_segaddress(gencodectx_t gctx, name_t *np, llvm_stgclass_t *segclass, int *signext);
LLVMValueRef llvmgen_expression(gencodectx_t gctx, expr_node_t *exp, LLVMTypeRef neededtype);
LLVMValueRef llvmgen_addr_expression(gencodectx_t gctx, expr_node_t *exp);
LLVMValueRef llvmgen_adjustval(gencodectx_t gctx, LLVMValueRef val, LLVMTypeRef neededtype);
void llvmgen_expgen_register(gencodectx_t gctx, exprtype_t type, llvmgen_expgen_fn func);
void llvmgen_opexpgen_init(gencodectx_t gctx);
void llvmgen_ctrlexpgen_init(gencodectx_t gctx);
void llvmgen_execfuncgen_init(gencodectx_t gctx);
void llvmgen_expgen_init(gencodectx_t gctx);
LLVMIntPredicate llvmgen_predfromop(optype_t op, int addrsigned);
#endif
