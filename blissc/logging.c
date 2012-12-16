//
//  logging.c
//  blissc
//
//  Created by Matthew Madison on 12/16/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "logging.h"

struct logctx_s {
    jmp_buf         retenv;
    unsigned int    maxerrs;
    unsigned int    infocount;
    unsigned int    warncount;
    unsigned int    errcount;
};

logctx_t
logging_init (jmp_buf retenv)
{
    logctx_t ctx = malloc(sizeof(struct logctx_s));

    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct logctx_s));
        memcpy(ctx->retenv, retenv, sizeof(jmp_buf));
    }
    return ctx;

} /* logging init */

void
logging_finish (logctx_t ctx)
{
    free(ctx);
}

unsigned int log_maxerrs(logctx_t ctx) { return ctx->maxerrs; }
void log_maxerrs_set(logctx_t ctx, unsigned int maxerrs) { ctx->maxerrs = maxerrs; }
unsigned int log_infcount(logctx_t ctx) { return ctx->infocount; }
unsigned int log_warncount(logctx_t ctx) { return ctx->warncount; }
unsigned int log_errcount(logctx_t ctx) { return ctx->errcount; }

void
log_signal (logctx_t ctx, statcode_t code, ...)
{
    va_list ap;
    char logbuf[256];
    int len;
    unsigned int sev = stc_severity(code);

    va_start(ap, code);
    len = stc_msg_vformat(code, logbuf, sizeof(logbuf)-1, ap);
    va_end(ap);
    logbuf[len] = '\0';
    fprintf(stderr, "%%%s\n", logbuf);
    switch (sev) {
        case STC_K_ERROR:
            ctx->errcount += 1;
            if (ctx->errcount < ctx->maxerrs) break;
            fprintf(stderr, "%%F, maximum number of errors exceeded, aborting\n");
            // FALLTHROUGH
        case STC_K_FATAL:
            fflush(stderr);
            longjmp(ctx->retenv, 1);
            break;
        case STC_K_WARN:
            ctx->warncount += 1;
            break;
        case STC_K_INFO:
            ctx->infocount += 1;
            break;
        default:
            break;
    }

} /* log_signal */

void
log_message (logctx_t ctx, strdesc_t *str)
{
    log_signal(ctx, STC__MESSAGE, str);
}
void
log_warn (logctx_t ctx, strdesc_t *str)
{
    log_signal(ctx, STC__USRWARN, str);
}

void
log_error (logctx_t ctx, strdesc_t *str)
{
    log_signal(ctx, STC__USRERR, str);
}

void
log_inform (logctx_t ctx, strdesc_t *str)
{
    log_signal(ctx, STC__INFORM, str);
}

void
log_print (logctx_t ctx, strdesc_t *str)
{
    printf("%% %-*.*s\n", str->len, str->len, str->ptr);
}