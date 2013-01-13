//
//  llvm_helper.h
//  blissc
//
//  Created by Matthew Madison on 1/5/13.
//  Copyright (c) 2013 Matthew Madison. All rights reserved.
//

#ifndef __blissc__llvm_helper__
#define __blissc__llvm_helper__

#ifdef __cplusplus

extern "C" {
#endif

    void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes);
    const char *HelperGetDefaultTriple(void);
    LLVMTargetRef HelperLookupTarget(const char *triple, char **err);

#ifdef __cplusplus
}
#endif
#endif /* defined(__blissc__llvm_helper__) */
