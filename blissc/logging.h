//
//  logging.h
//  blissc
//
//  Created by Matthew Madison on 12/16/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_logging_h
#define blissc_logging_h

#include <setjmp.h>
#include "strings.h"
#include "statcodes.h"

struct logctx_s;
typedef struct logctx_s *logctx_t;

#define LOG_MAXERRS_DEFAULT 10

logctx_t logging_init(jmp_buf returnpoint);
unsigned int log_maxerrs(logctx_t ctx);
void log_maxerrs_set(logctx_t ctx, unsigned int maxerrs);
unsigned int log_infocount(logctx_t ctx);
unsigned int log_warncount(logctx_t ctx);
unsigned int log_errcount(logctx_t ctx);
void log_signal(logctx_t ctx, statcode_t code, ...);
void log_message(logctx_t ctx, strdesc_t *str);
void log_warn(logctx_t ctx, strdesc_t *str);
void log_error(logctx_t ctx, strdesc_t *str);
void log_inform(logctx_t ctx, strdesc_t *str);
void log_print(logctx_t ctx, strdesc_t *str);
void logging_finish(logctx_t ctx);
#endif
