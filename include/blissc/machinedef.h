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

#define MACH_M_ADDRS_SIGNED (1<<0)
#define MACH_M_SIGNEXT      (1<<1)
#define MACH_M_LTC_INIT     (1<<2)

#define MACH_K_MAXCSCOUNT   4

struct machine_ctx_s;
typedef struct machine_ctx_s *machine_ctx_t;

struct machinedef_s {
    machine_ctx_t   machctx;
    unsigned int    bpunit, bpval, bpaddr;
    unsigned int    charsize_count;
    unsigned int    charsizes[MACH_K_MAXCSCOUNT];
    unsigned int    max_align; // maximum # of bits for ALIGN() attribute
    unsigned int    reg_count;
    unsigned int    flags;
};

typedef struct machinedef_s machinedef_t;

#define siu static inline __attribute__((unused))
siu unsigned int machine_unit_bits(machinedef_t *mach) { return mach->bpunit; }
siu unsigned int machine_scalar_bits(machinedef_t *mach) { return mach->bpval; }
siu unsigned int machine_scalar_units(machinedef_t *mach) { return mach->bpval/mach->bpunit; }
siu unsigned int machine_addr_bits(machinedef_t *mach) { return mach->bpaddr; }
siu unsigned int machine_addr_units(machinedef_t *mach) { return mach->bpaddr/mach->bpunit; }
siu int machine_addr_signed(machinedef_t *mach) { return (mach->flags & MACH_M_ADDRS_SIGNED) != 0; }
siu int machine_signext_supported(machinedef_t *mach) { return (mach->flags & MACH_M_SIGNEXT) != 0; }
// The following for host compiler to target machine mapping
siu unsigned int machine_unit_maxbytes(machinedef_t *mach) { return mach->bpunit/8; }
siu unsigned int machine_scalar_maxbytes(machinedef_t *mach) { return mach->bpval/8; }
siu unsigned int machine_addr_maxbytes(machinedef_t *mach) { return mach->bpaddr/8; }
siu unsigned int machine_register_count(machinedef_t *mach) { return mach->reg_count; }
siu unsigned int machine_align_max(machinedef_t *mach) { return mach->max_align; }
siu int machine_linktime_constant_initializers(machinedef_t *mach) {
    return (mach->flags & MACH_M_LTC_INIT) != 0; }
siu machine_ctx_t machine_context(machinedef_t *mach) { return mach->machctx; }
siu unsigned int machine_cs_count(machinedef_t *mach) { return mach->charsize_count; }
siu unsigned int machine_charsize(machinedef_t *mach, int idx) { return mach->charsizes[idx]; }
#undef siu

machinedef_t *machine_init (const char *machspec);

#endif /* machinedef_h__ */
