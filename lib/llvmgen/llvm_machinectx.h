#ifndef llvm_machinectx_h__
#define llvm_machinectx_h__
/*
 *++
 *	File:			llvm_machinectx.h
 *
 *	Abstract:		LLVM "machine" context.
 *
 *	Author:			M. Madison
 *					Copyright © 2013, Matthew Madison
 *					All rights reserved.
 *--
 */

#include "blissc/machinedef.h"
#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm_helper.h"

struct machine_ctx_s {
    void                   *genctx;
    LLVMContextRef          llvmctx;
    LLVMTargetMachineRef    target_machine;
};


#endif /* llvm_machinectx_h__ */
