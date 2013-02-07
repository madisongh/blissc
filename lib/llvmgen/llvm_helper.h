#ifndef llvm_helper_h__
#define llvm_helper_h__
/*
 *++
 *	File:			llvm_helper.h
 *
 *	Abstract:		Additional LLVM C APIs
 *
 *	Author:			M. Madison
 *					Copyright © 2013, Matthew Madison
 *					All rights reserved.
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
