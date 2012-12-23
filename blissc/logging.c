/*
 *++
 *	File:			logging.c
 *
 *	Abstract:		Logging/error handling facility.
 *
 *  Module description:
 *		This module implements logging of diagnostic messages,
 *		based on the status code definitions (see statcodes.c/h).
 *		When a fatal error is encountered, or the number of
 *		non-fatal errors exceeds the maximum-error threshold,
 *		compilation is aborted via longjmp().  Otherwise, diagnostics
 *		are simply formatted and logged to stderr with no
 *		interruption in flow.
 *
 *		Text position will be reported with any diagnostic message,
 *		if possible; for this to work, a function pointer must be
 *		provided for this module to call to map the filename index
 *		in the textpos_t type to an actual file name.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		22-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "logging.h"
#include "utils.h"

struct logctx_s {
    jmp_buf             retenv;
    filename_fetch_fn   fetchfn;
    void                *ffctx;
    unsigned int        maxerrs;
    unsigned int        infocount;
    unsigned int        warncount;
    unsigned int        errcount;
};

/*
 * logging_init
 *
 * Module initialization.  The caller is expected to provide
 * the jmp_buf for us to longjmp() to when we need to abort.
 */
logctx_t
logging_init (jmp_buf retenv)
{
    logctx_t ctx = malloc(sizeof(struct logctx_s));

    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct logctx_s));
        memcpy(ctx->retenv, retenv, sizeof(jmp_buf));
        ctx->maxerrs = LOG_MAXERRS_DEFAULT;
    }
    return ctx;

} /* logging init */

/*
 * logging_finish
 *
 * Frees up the logging context.
 */
void
logging_finish (logctx_t ctx)
{
    free(ctx);

} /* logging_finish */

/*
 * Getters/setters for logging context
 */
unsigned int log_maxerrs(logctx_t ctx) { return ctx->maxerrs; }
void log_maxerrs_set(logctx_t ctx, unsigned int maxerrs) { ctx->maxerrs = maxerrs; }
unsigned int log_infcount(logctx_t ctx) { return ctx->infocount; }
unsigned int log_warncount(logctx_t ctx) { return ctx->warncount; }
unsigned int log_errcount(logctx_t ctx) { return ctx->errcount; }
void log_fetchfn_set(logctx_t ctx, filename_fetch_fn ffn, void *ffctx) {
    ctx->fetchfn = ffn; ctx->ffctx = ffctx; }

/*
 * log_vsignal
 *
 * Core logging function.  Formats the diagnostic message and text
 * position (if possible), emits the message on stderr, and handles
 * the next step - incrementing the appropriate diagnostic counter
 * and possibly aborting.
 */
void
log_vsignal (logctx_t ctx, textpos_t pos, statcode_t code, va_list ap)
{
    char logbuf[256];
    int len;
    unsigned int sev = stc_severity(code);

    len = stc_msg_vformat(code, logbuf, sizeof(logbuf)-1, ap);
    logbuf[len] = '\0';
    fprintf(stderr, "%s\n", logbuf);
    if (pos != 0 && ctx->fetchfn != 0) {
        strdesc_t *fname = ctx->fetchfn(ctx->ffctx, textpos_fileno(pos));
        if (fname != 0) {
            fprintf(stderr, "-  at %-*.*s:%u:%u\n",
                    fname->len, fname->len, fname->ptr,
                    textpos_lineno(pos), textpos_colnum(pos));
        }
    }
    switch (sev) {
        case STC_K_ERROR:
            ctx->errcount += 1;
            if (ctx->errcount < ctx->maxerrs) break;
            fprintf(stderr, "%%BLISS-F-TOOMANYERRS, "
            	    "maximum number of errors exceeded, aborting\n");
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

} /* log_vsignal */

/*
 * log_signal
 *
 * The varargs API for logging.
 */
void
log_signal (logctx_t ctx, textpos_t pos, statcode_t code, ...)
{
    va_list ap;
    va_start(ap, code);
    log_vsignal(ctx, pos, code, ap);
    va_end(ap);

} /* log_signal */

/*
 * log_print
 *
 * XXX this function should be moved to the listing module.
 */
void
log_print (logctx_t ctx, textpos_t pos, strdesc_t *str)
{
    printf("%% %-*.*s\n", str->len, str->len, str->ptr);

} /* log_print */
