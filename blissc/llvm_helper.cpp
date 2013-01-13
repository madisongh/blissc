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
#pragma clang diagnostic pop
#include "llvm_helper.h"
#include <cstdio>
#include <string>

using namespace llvm;

void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes) {
    unwrap<AllocaInst>(Inst)->setAlignment(Bytes);
}
