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
 * Copyright © 2013-2024, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "llvm-c/TargetMachine.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm_helper.h"
#include "llvm/IR/Instructions.h"

#if LLVM_VERSION_MAJOR >= 14
#include "llvm/MC/TargetRegistry.h"
#else
#include "llvm/Support/TargetRegistry.h"
#endif
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Host.h"

using namespace llvm;

void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes) {
    reinterpret_cast<AllocaInst*>(Inst)->setAlignment(Align(Bytes));
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
    reinterpret_cast<TargetMachine*>(tm)->Options.MCOptions.AsmVerbose = (bool) v;
}
