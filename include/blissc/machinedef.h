#ifndef machinedef_h__
#define machinedef_h__
/*
 *++
 *	File:			machinedef.h
 *
 *	Abstract:		Machine-specific definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */

// Maximum number of registers on any supported platform
#define MACH_K_MAXREGS  32

struct machinedef_s {
    unsigned int    bpunit, bpval, bpaddr;
    int             addr_signed; // true if addresses are sign-extended
    int             signext_supported;
    unsigned int    max_align; // maximum # of bits for ALIGN() attribute
    unsigned int    reg_count;
    int             ltc_initializers;
};

typedef struct machinedef_s machinedef_t;

#define siu static inline __attribute__((unused))
siu unsigned int machine_unit_bits(machinedef_t *mach) { return mach->bpunit; }
siu unsigned int machine_scalar_bits(machinedef_t *mach) { return mach->bpval; }
siu unsigned int machine_scalar_units(machinedef_t *mach) { return mach->bpval/mach->bpunit; }
siu unsigned int machine_addr_bits(machinedef_t *mach) { return mach->bpaddr; }
siu unsigned int machine_addr_units(machinedef_t *mach) { return mach->bpaddr/mach->bpunit; }
siu int machine_addr_signed(machinedef_t *mach) { return mach->addr_signed; }
siu int machine_signext_supported(machinedef_t *mach) { return mach->signext_supported; }
// The following for host compiler to target machine mapping
siu unsigned int machine_unit_maxbytes(machinedef_t *mach) { return mach->bpunit/8; }
siu unsigned int machine_scalar_maxbytes(machinedef_t *mach) { return mach->bpval/8; }
siu unsigned int machine_addr_maxbytes(machinedef_t *mach) { return mach->bpaddr/8; }
siu unsigned int machine_register_count(machinedef_t *mach) { return mach->reg_count; }
siu unsigned int machine_align_max(machinedef_t *mach) { return mach->max_align; }
siu int machine_linktime_constant_initializers(machinedef_t *mach) {
    return mach->ltc_initializers; }
#undef siu

#endif /* machinedef_h__ */
