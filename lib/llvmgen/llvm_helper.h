#ifndef llvm_helper_h__
#define llvm_helper_h__
/*
 *++
 * llvm_helper.h - Additional LLVM C APIs
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#ifdef __cplusplus

extern "C" {
#endif

    void HelperSetAllocaAlignment(LLVMValueRef Inst, unsigned int Bytes);
    char *HelperGetDefaultTriple(void);
    LLVMTargetRef HelperLookupTarget(const char *triple, char **err);
    void HelperSetAsmVerbosity(LLVMTargetMachineRef tm, LLVMBool v);

#ifdef __cplusplus
}
#endif
#endif /* llvm_helper_h__ */
