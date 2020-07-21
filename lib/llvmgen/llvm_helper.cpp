/*
 *++
 * llvm_helper.cpp - Implements C APIs for parts of LLVM.
 *
 *
 * LLVM provides C-callable APIs for most, but not all,
 * of the functionality we need.  This module contains
 * helper functions to provide C APIs for the handful
 * of other functions not already provided.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/Target/TargetMachine.h"
#pragma clang diagnostic pop
#include "llvm_helper.h"
#if LLVM_VERSION_MAJOR > 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >=  3)
#include "llvm/IR/Instructions.h"
#endif

#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes) {
    reinterpret_cast<AllocaInst*>(Inst)->setAlignment(llvm::MaybeAlign(Bytes));
}

char *HelperGetDefaultTriple(void) {
    return strdup(sys::getDefaultTargetTriple().c_str());
}

LLVMTargetRef HelperLookupTarget(const char *triple, char **err) {
    std::string error;
    const Target* target = TargetRegistry::lookupTarget(triple, error);
    if (!error.empty()) *err = strdup(error.c_str());
    return reinterpret_cast<LLVMTargetRef>(const_cast<Target*>(target));
}

void HelperSetAsmVerbosity(LLVMTargetMachineRef tm, LLVMBool v) {
#if LLVM_VERSION_MAJOR > 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 6)
    reinterpret_cast<TargetMachine*>(tm)->Options.MCOptions.AsmVerbose = v;
#else
    reinterpret_cast<TargetMachine*>(tm)->setAsmVerbosityDefault(v);
#endif
}
