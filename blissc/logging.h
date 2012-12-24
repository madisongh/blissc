#ifndef logging_h__
#define logging_h__
/*
 *++
 *	File:			logging.h
 *
 *	Abstract:		Logging definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include <setjmp.h>
#include <stdarg.h>
#include "strings.h"
#include "statcodes.h"
#include "utils.h"

struct logctx_s;
typedef struct logctx_s *logctx_t;
typedef strdesc_t * (*filename_fetch_fn)(void *, unsigned int);

#define LOG_MAXERRS_DEFAULT 10

logctx_t logging_init(jmp_buf returnpoint);
unsigned int log_maxerrs(logctx_t ctx);
void log_maxerrs_set(logctx_t ctx, unsigned int maxerrs);
void log_fetchfn_set(logctx_t ctx, filename_fetch_fn fn, void *ffctx);
unsigned int log_infocount(logctx_t ctx);
unsigned int log_warncount(logctx_t ctx);
unsigned int log_errcount(logctx_t ctx);
void log_signal(logctx_t ctx, textpos_t pos, statcode_t code, ...);
void log_vsignal(logctx_t ctx, textpos_t pos, statcode_t code, va_list ap);
void log_print(logctx_t ctx, textpos_t pos, const char *s, size_t len);
void logging_finish(logctx_t ctx);

#endif /* logging_h__ */
