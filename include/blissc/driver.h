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

blissc_driverctx_t blissc_init(jmp_buf retenv);
int blissc_target_set(blissc_driverctx_t ctx, const char *machspec);
int blissc_compile(blissc_driverctx_t ctx, const char *fname, int fnlen);
void blissc_finish(blissc_driverctx_t ctx);



#endif /* blissc_driver_h__ */
