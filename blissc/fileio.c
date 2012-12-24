/*
 *++
 *	File:			fileio.c
 *
 *	Abstract:		File input and output handling.
 *
 *  Module description:
 *		This module implements all file I/O handling.
 *		XXX At the moment, it's only "I".
 *
 *		The implementation here uses standard C I/O
 *		calls, but abstracts the I/O handling such that
 *		some OS-specific implementation could be used
 *		if needed (e.g., for performance reasons).
 *
 *		Input files are assumed to be text divided up
 *		into lines ending with linemarks (for standard
 *		C I/O, '\n').  Input is fetched one line at a
 *		time.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		22-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "fileio.h"
#include "logging.h"
#include <errno.h>

// Per-file context
struct filectx_s {
    struct filectx_s *next;
    fioctx_t          fio;
    int               fd;
    size_t            fnlen;
    size_t            bufpos;
    size_t            buflen;
    char              *fname;
    char              filebuf[4096];
};

// Module context
struct fioctx_s {
    logctx_t         logctx;
    filectx_t        input_files;
};


/*
 * fileio_init
 *
 * Module initialization.
 */
fioctx_t
fileio_init (logctx_t logctx)
{
    fioctx_t fio = malloc(sizeof(struct fioctx_s));
    if (fio != 0) {
        memset(fio, 0, sizeof(struct fioctx_s));
        fio->logctx = logctx;
    }
    return fio;

} /* fileio_init */

/*
 * fileio_finish
 *
 * Shutdown routine.
 */
void
fileio_finish (fioctx_t fio)
{
    filectx_t ctx, cnext;

    for (ctx = fio->input_files; ctx != 0; ctx = cnext) {
        cnext = ctx->next;
        close(ctx->fd);
        free(ctx);
    }

    free(fio);

} /* fileio_finish */

/*
 * file_open_input
 *
 * Opens an input file with the specified name, replacing the
 * suffix of the original name with the specified suffix.  If
 * the original name does not have a suffix, the specified
 * suffix is simply appended.
 */
filectx_t
file_open_input (fioctx_t fio, const char *fname, size_t fnlen,
                 const char *suffix)
{
    filectx_t ctx = malloc(sizeof(struct filectx_s));
    int add_suffix = 0;

    if (ctx == 0) {
        return ctx;
	}
    memset(ctx, 0, sizeof(struct filectx_s));

    if (suffix != 0) {
		if (fnlen == 0) {
			ctx->fname = malloc(strlen(suffix)+1);
			add_suffix = 1;
		} else {
			const char *cp = fname + fnlen;
			while (--cp >= fname && *cp != '.');
			add_suffix = !(cp >= fname);
			ctx->fname = malloc(fnlen + (add_suffix ? strlen(suffix) : 0) + 1);
		}
	} else if (fnlen == 0) {
		ctx->fname = 0;
	} else {
		ctx->fname = malloc(fnlen+1);
	}
    if (ctx->fname == 0) {
        free(ctx);
        return 0;
    }
    memcpy(ctx->fname, fname, fnlen);
    if (add_suffix) {
        strcpy(ctx->fname+fnlen, suffix);
    } else {
        ctx->fname[fnlen] = '\0';
    }
    ctx->fnlen = fnlen + (add_suffix ? strlen(suffix) : 0);
    ctx->fd = open(ctx->fname, O_RDONLY);
    if (ctx->fd < 0) {
        char errbuf[64];
        errbuf[0] = '\0';
        strerror_r(errno, errbuf, sizeof(errbuf));
        log_signal(fio->logctx, 0, STC__OPENERR, ctx->fname, ctx->fnlen, errbuf);
        free(ctx);
        return 0;
    }
    ctx->next = fio->input_files;
    fio->input_files = ctx;
    ctx->bufpos = 0;
    ctx->buflen = 0;
    ctx->fio = fio;
    return ctx;

} /* file_open_input */

/*
 * file_close
 *
 * Close a file.
 */
void
file_close (filectx_t ctx)
{
    fioctx_t fio;
    filectx_t prev, cur;

    if (ctx == 0 || ctx->fio == 0) {
        return;
    }
    fio = ctx->fio;
    prev = 0;
    for (cur = fio->input_files; cur != 0; cur = cur->next) {
        if (cur == ctx)
            break;
        prev = cur;
    }
    if (cur == 0) {
        log_signal(fio->logctx, 0, STC__INTCMPERR, "file_close");
        return;
    }
    if (prev == 0) {
        fio->input_files = cur->next;
    } else {
        prev->next = cur->next;
    }
    close(cur->fd);
    if (cur->fname != 0) {
    	free(cur->fname);
    }
    free(cur);
}

/*
 * file_getname
 *
 * Returns the name of an open file.
 */
char *
file_getname (filectx_t ctx)
{
    return ctx->fname;

} /* file_getname */

/*
 * file_readline
 *
 * Reads a line from a file.
 *
 * Returns:
 *		-1: error occurred (errors are also signalled)
 *		 0: end of file reached, no input
 *		>0: length of line
 */
int
file_readline (filectx_t ctx, char *buf, size_t bufsiz, size_t *len)
{
    char *outp = buf;
    char *origbp = &ctx->filebuf[ctx->bufpos];
    char *bp = origbp;
    char *cp;
    size_t remain = ctx->buflen - ctx->bufpos;
    ssize_t ret;
    int status = 0;

    while (bufsiz > 0) {
        if (remain > 0) {
            cp = memchr(bp, '\n', remain);
            if (cp != 0) {
                size_t count = cp - bp;
                if (count > bufsiz) {
                    log_signal(ctx->fio->logctx, 0, STC__LNTOOLONG, file_getname(ctx));
                    status = -1;
                    break;
                }
                memcpy(outp, bp, count);
                *len = (outp - buf) + count;
                ctx->bufpos += (cp - origbp) + 1;
                status = 1;
                break;
            }
            if (remain > bufsiz) {
                log_signal(ctx->fio->logctx, 0, STC__LNTOOLONG, file_getname(ctx));
                status = -1;
                break;
            }
            memcpy(outp, bp, remain);
            outp += remain;
            bufsiz -= remain;
        }
        ret = read(ctx->fd, ctx->filebuf, sizeof(ctx->filebuf));
        if (ret < 0) {
            char errbuf[64];
            errbuf[0] = '\0';
            strerror_r(errno, errbuf, sizeof(errbuf));
            log_signal(ctx->fio->logctx, 0, STC__FIOERR, errbuf);
            status = -1;
            break;
        }
        ctx->buflen = ret;
        ctx->bufpos = 0;
        if (ret == 0) {
            if (outp == buf) {
                status = 0;
            } else {
                *len = outp - buf;
                status = 1;
            }
            break;
        }
        remain = ctx->buflen;
        origbp = bp = &ctx->filebuf[0];
    }

    return status;

} /* file_readline */
