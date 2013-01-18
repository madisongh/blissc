//
//  llvm_helper.cpp
//  blissc
//
//  Created by Matthew Madison on 1/5/13.
//  Copyright (c) 2013 Matthew Madison. All rights reserved.
//

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/Target/TargetMachine.h"
#pragma clang diagnostic pop
#include "llvm_helper.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include <cstdio>
#include <string>

using namespace llvm;

void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes) {
    unwrap<AllocaInst>(Inst)->setAlignment(Bytes);
}

const char *HelperGetDefaultTriple(void) {
    return sys::getDefaultTargetTriple().c_str();
}

LLVMTargetRef HelperLookupTarget(const char *triple, char **err) {
    std::string error;
    const Target* target = TargetRegistry::lookupTarget(triple, error);
    if (!error.empty()) *err = strdup(error.c_str());
    return wrap(target);
}

void HelperSetAsmVerbosity(LLVMTargetMachineRef tm, LLVMBool v) {
    unwrap(tm)->setAsmVerbosityDefault(v);
}