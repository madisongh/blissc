//
//  machinedef.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_machinedef_h
#define blissc_machinedef_h

struct machinedef_s {
    unsigned int    bpunit, bpval, bpaddr;
    int             addr_signed; // true if addresses are sign-extended
    int             signext_supported;
};

typedef struct machinedef_s machinedef_t;

#define siu static inline __unused
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
#undef siu

#endif
