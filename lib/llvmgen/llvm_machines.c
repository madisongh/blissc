/*
 *++
 *	File:			llvm_machines.c
 *
 *	Abstract:		Maps LLVM targets to BLISS machines.
 *
 *  Module description:
 *
 *
 *	Author:		M. Madison
 *				Copyright © 2013, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		18-Jan-2013	V1.0	Madison		Initial coding.
 *--
 */

#include "llvm_machinectx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

machinedef_t *
machine_init (const char *machspec)
{
    machine_ctx_t m = malloc(sizeof(struct machine_ctx_s) + sizeof(struct machinedef_s));
    machinedef_t *mach;
    char default_triple[64], *err;
    LLVMTargetRef target;

    if (m == 0) return 0;
    memset(m, 0, sizeof(struct machine_ctx_s) + sizeof(struct machinedef_s));

    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();

    if (machspec == 0) {
        strcpy(default_triple, HelperGetDefaultTriple());
        machspec = default_triple;
    }
    err = 0;
    target = HelperLookupTarget(machspec, &err);
    if (target == 0) {
        if (err != 0) free(err);
        free(m);
        return 0;
    }
    m->target_machine = LLVMCreateTargetMachine(target, (char *)machspec, "", "",
                                                LLVMCodeGenLevelDefault,
                                                LLVMRelocPIC, LLVMCodeModelDefault);
    if (m->target_machine == 0) {
        free(m);
        return 0;
    }
    HelperSetAsmVerbosity(m->target_machine, 1);
    m->llvmctx = LLVMContextCreate();
    if (m->llvmctx == 0) {
        LLVMDisposeTargetMachine(m->target_machine);
        free(m);
        return 0;
    }

    mach = (machinedef_t *)(m + 1);

    mach->machctx = m;
    mach->bpaddr = 64;
    mach->bpval  = 64;
    mach->bpunit = 8;
    mach->charsize_count = 1;
    mach->charsizes[0] = 8;
    mach->flags = MACH_M_SIGNEXT | MACH_M_LTC_INIT;
    mach->max_align = 4;
    mach->reg_count = 16;

    return mach;
    
} /* machine_init */

void
machine_output_set (machinedef_t *mach, machine_output_t outtype, char *fname, int fnlen)
{
    machine_ctx_t m = machine_context(mach);

    m->outputtype = (outtype == MACH_K_OUTPUT_ASM ? LLVMAssemblyFile : LLVMObjectFile);
    if (fnlen < 0) {
        fnlen = (int) strlen(fname);
    }
    m->outfile = malloc(fnlen+1);
    memcpy(m->outfile, fname, fnlen);
    fname[fnlen] = '\0';

} /* machine_output_set */

void
machine_finish (machinedef_t *mach)
{
    machine_ctx_t m;

    if (mach == 0) return;
    m = machine_context(mach);
    if (m == 0) return;

    if (m->target_machine != 0) LLVMDisposeTargetMachine(m->target_machine);
    if (m->llvmctx != 0) LLVMContextDispose(m->llvmctx);
    free(m);
    
} /* machine_finish */

