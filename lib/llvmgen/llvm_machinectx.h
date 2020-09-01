#ifndef llvm_machinectx_h__
#define llvm_machinectx_h__
/*
 *++
 * llvm_machinectx.h - LLVM "machine" context.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "blissc/machinedef.h"
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm-c/Transforms/AggressiveInstCombine.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm_helper.h"

struct machine_ctx_s {
    void                   *genctx;
    LLVMContextRef          llvmctx;
    LLVMTargetMachineRef    target_machine;
    LLVMCodeGenFileType     outputtype;
    char                    *outfile;
    char                    *irdumpfile;
    char                    *triple;
    int                     is_macho;
};

#endif /* llvm_machinectx_h__ */
