#ifndef blissc_driver_h__
#define blissc_driver_h__
/*
 *++
 *	File:			driver.h
 *
 *	Abstract:		compiler "driver" interface
 *
 *	Author:			M. Madison
 *					Copyright © 2013, Matthew Madison
 *					All rights reserved.
 *--
 */

#include <setjmp.h>

struct blissc_driverctx_s;
typedef struct blissc_driverctx_s *blissc_driverctx_t;

typedef enum {
    BLISS_K_OUTPUT_ASSEMBLY,
    BLISS_K_OUTPUT_OBJECT
} bliss_output_t;

// XXX - bit positions must match the enum in
//       listings.h
#define BLISS_M_LIST_SRC    (1<<0)
#define BLISS_M_LIST_REQ    (1<<1)
#define BLISS_M_LIST_EXP    (1<<2)
#define BLISS_M_LIST_TRC    (1<<3)
#define BLISS_M_LIST_LIB    (1<<4)
#define BLISS_M_LIST_OBJ    (1<<5)
#define BLISS_M_LIST_ASM    (1<<6)
#define BLISS_M_LIST_BIN    (1<<7)
#define BLISS_M_LIST_COM    (1<<8)

blissc_driverctx_t blissc_init(jmp_buf retenv);
int blissc_target_set(blissc_driverctx_t ctx, const char *machspec);
int blissc_output_set(blissc_driverctx_t ctx, bliss_output_t outtype,
                      const char *fname, int fnlen);
int blissc_listopt_set(blissc_driverctx_t ctx, unsigned int listflags,
                       const char *fname, int fnlen);
int blissc_variant_set(blissc_driverctx_t ctx, unsigned int val);
int blissc_optlevel_set(blissc_driverctx_t ctx, unsigned int val);
int blissc_compile(blissc_driverctx_t ctx, const char *fname, int fnlen);
void blissc_finish(blissc_driverctx_t ctx);



#endif /* blissc_driver_h__ */
