/*
 *++
 * logging.c - Logging/error handling facility.
 *
 * This module implements logging of diagnostic messages,
 * based on the status code definitions (see statcodes.c/h).
 * When a fatal error is encountered, or the number of
 * non-fatal errors exceeds the maximum-error threshold,
 * compilation is aborted via longjmp().  Otherwise, diagnostics
 * are simply formatted and logged to stderr with no
 * interruption in flow.
 *
 * Text position will be reported with any diagnostic message,
 * if possible; for this to work, a function pointer must be
 * provided for this module to call to map the filename index
 * in the textpos_t type to an actual file name.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "blissc/support/logging.h"

struct logctx_s {
    jmp_buf             retenv;
    filename_fetch_fn   fetchfn;
    line_fetch_fn       linefetchfn;
    lstg_print_fn       listprintfn;
    void                *ffctx;
    void                *lfctx;
    void                *lpctx;
    unsigned int        maxerrs;
    unsigned int        infocount;
    unsigned int        warncount;
    unsigned int        errcount;
    int                 logtoterm;
    int                 logsrctolst;
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
        ctx->logtoterm = 1;
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
void log_linefetchfn_set(logctx_t ctx, line_fetch_fn lfn, void *lfctx) {
    ctx->linefetchfn = lfn; ctx->lfctx = lfctx; }
void log_lstgprintfn_set(logctx_t ctx, lstg_print_fn lpfn, void *lpctx) {
    ctx->listprintfn = lpfn; ctx->lpctx = lpctx; }
void log_logterm_set (logctx_t ctx, int val) { ctx->logtoterm = val; }
void log_logsrctolst_set (logctx_t ctx, int val) { ctx->logsrctolst = val; }

/*
 * Print to both listing file and terminal
 */
static void
doprint (logctx_t ctx, const char *buf, size_t buflen, int align_with_source)
{
    if (ctx->listprintfn != 0) {
        (*ctx->listprintfn)(ctx->lpctx, buf, buflen, align_with_source);
    }
    if (ctx->logtoterm) {
        fprintf(stderr, "%-*.*s\n", (int) buflen, (int) buflen, buf);
    }
}
static void
doprintsrc (logctx_t ctx, const char *buf, size_t buflen)
{
    if (ctx->logsrctolst && ctx->listprintfn != 0) {
        (*ctx->listprintfn)(ctx->lpctx, buf, buflen, 0);
    }
    if (ctx->logtoterm) {
        fprintf(stderr, "%-*.*s\n", (int) buflen, (int) buflen, buf);
    }
}

/*
 * log_message
 *
 * Issue a message to the terminal (for %MESSAGE)
 */
void
log_message (logctx_t ctx, const char *buf, size_t buflen)
{
    fprintf(stderr, "%% %-*.*s\n", (int) buflen, (int) buflen, buf);

} /* log_message */

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

    if (sev == STC_K_FATAL) {
        ctx->logtoterm = 1;
    }

    if (ctx->listprintfn != 0 || ctx->logtoterm) {
        int fileno = textpos_fileno(pos);
        unsigned int lineno = textpos_lineno(pos);
        unsigned int colno  = textpos_colnum(pos);
        if (ctx->linefetchfn != 0) {
            strdesc_t *curline = (*ctx->linefetchfn)(ctx->lfctx, fileno, lineno);
            if (curline != 0) {
                doprintsrc(ctx, curline->ptr, curline->len);
                if (curline->len < sizeof(logbuf) && colno < curline->len) {
                    memset(logbuf, '.', colno);
                    logbuf[colno] = '|';
                    doprint(ctx, logbuf, colno+1, !ctx->logsrctolst);
                }
            }
        }
        len = stc_msg_vformat(code, logbuf, sizeof(logbuf)-1, ap);
        doprint(ctx, logbuf, len, 0);
        if (pos != 0 && ctx->fetchfn != 0) {
            strdesc_t *fname = ctx->fetchfn(ctx->ffctx, fileno);
            if (fname != 0)  {
                len = snprintf(logbuf, sizeof(logbuf)-1, "-  at %-*.*s:%u:%u",
                               (int) fname->len, (int) fname->len, fname->ptr, lineno, colno+1);
                if (len > 0) doprint(ctx, logbuf, len, 0);
            }
        }
    }
    switch (sev) {
        case STC_K_ERROR:
            ctx->errcount += 1;
            if (ctx->errcount < ctx->maxerrs) break;
            // Force this error message to the terminal
            ctx->logtoterm = 1;
            len = snprintf(logbuf, sizeof(logbuf)-1, "%%BLISS-F-TOOMANYERRS, "
                           "maximum number of errors exceeded, aborting");
            if (len > 0) doprint(ctx, logbuf, len, 0);
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
