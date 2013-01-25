/*
 *++
 *	File:			fileio.c
 *
 *	Abstract:		File input and output handling.
 *
 *  Module description:
 *		This module implements all file I/O and path
 *      name handling.
 *
 *		The implementation here uses standard C I/O
 *		calls, but abstracts the I/O handling such that
 *		some OS-specific implementation could be used
 *		if needed (e.g., for performance reasons).
 *
 *		The readline/writline routines assume that files
 *      are text divided into lines ending with linemarks
 *      (for standard C I/O, '\n').
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
#include <sys/uio.h>
#include <libgen.h>
#include "blissc/support/fileio.h"
#include "blissc/support/logging.h"
#include <errno.h>

// Per-file context
struct filectx_s {
    struct filectx_s *next;
    fioctx_t          fio;
    int               fd;
    int               is_output;
    size_t            fnlen;
    size_t            bufpos;
    size_t            buflen;
    char              *fname;
    char              filebuf[4096];
};

// Module context
struct fioctx_s {
    logctx_t         logctx;
    filectx_t        open_files;
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

    for (ctx = fio->open_files; ctx != 0; ctx = cnext) {
        cnext = ctx->next;
        close(ctx->fd);
        if (ctx->fname != 0) free(ctx->fname);
        free(ctx);
    }

    free(fio);

} /* fileio_finish */

char *
file_canonicalname (fioctx_t fio, const char *orig, int origlen, unsigned int *lenp)
{
    char *rpath;

    if (origlen < 0) {
        rpath = realpath(orig, 0);
    } else {
        char *ocopy = malloc((unsigned)(origlen+1));
        if (ocopy == 0) return 0;
        memcpy(ocopy, orig, origlen);
        ocopy[origlen] = '\0';
        rpath = realpath(ocopy, 0);
        free(ocopy);
    }
    if (rpath != 0 && lenp != 0) *lenp = (unsigned int)strlen(rpath);
    return rpath;
}

int
file_splitname (fioctx_t fio, const char *orig, int origlen, int canonicalize,
                fio_pathparts_t *parts)
{
    unsigned int len, partlen;
    char *cp;

    memset(parts, 0, sizeof(fio_pathparts_t));
    if (canonicalize) {
        parts->path_fullname = file_canonicalname(fio, orig, origlen, &len);
        if (parts->path_fullname == 0) return 0;
        parts->path_fullnamelen = len;
    } else {
        if (origlen < 0) origlen = (int) strlen(orig);
        parts->path_fullname = malloc((unsigned int)origlen);
        if (parts->path_fullname == 0) return 0;
        memcpy(parts->path_fullname, orig, origlen);
        parts->path_fullname[origlen] = '\0';
        len = (unsigned int) origlen;
    }
    for (cp = parts->path_fullname + (len-1), partlen = 0;
         cp >= parts->path_fullname && *cp != '.' && *cp != '/';
         cp -= 1, partlen += 1);
    if (cp >= parts->path_fullname) {
        if (*cp == '.') {
            parts->path_suffix = cp;
            parts->path_suffixlen = partlen+1;
            len = (unsigned int)(cp - parts->path_fullname);
        }
    }
    for (cp = parts->path_fullname + (len-1), partlen = 0;
         cp >= parts->path_fullname && *cp != '/';
         cp -= 1, partlen += 1);
    if (cp >= parts->path_fullname) {
        parts->path_filename = cp + 1;
        parts->path_filenamelen = partlen;
        parts->path_dirname = parts->path_fullname;
        parts->path_dirnamelen = (unsigned int) (cp - parts->path_fullname) + 1;
    } else {
        parts->path_filename = parts->path_fullname;
        parts->path_filenamelen = len;
    }

    return 1;
}

int
file_combinename (fioctx_t fio, fio_pathparts_t *parts)
{
    size_t len;
    if (parts->path_fullname != 0) free(parts->path_fullname);
    parts->path_fullname = malloc(parts->path_dirnamelen + parts->path_filenamelen +
                                  parts->path_suffixlen + 1);
    if (parts->path_fullname == 0) return 0;
    len = 0;
    if (parts->path_dirname != 0) {
        memcpy(parts->path_fullname, parts->path_dirname, parts->path_dirnamelen);
        len += parts->path_dirnamelen;
    }
    if (parts->path_filename != 0) {
        memcpy(parts->path_fullname + len, parts->path_filename, parts->path_filenamelen);
        len += parts->path_filenamelen;
    }
    if (parts->path_suffix != 0) {
        memcpy(parts->path_fullname + len, parts->path_suffix, parts->path_suffixlen);
        len += parts->path_suffixlen;
    }
    parts->path_fullname[len] = '\0';
    parts->path_fullnamelen = len;
    return 1;
}

void
file_freeparts (fioctx_t fio, fio_pathparts_t *parts)
{
    if (parts->path_fullname != 0) {
        free(parts->path_fullname);
    }
    memset(parts, 0, sizeof(fio_pathparts_t));
}

/*
 * file_open
 *
 * Opens a file with the specified name.
 */
static filectx_t
file_open (fioctx_t fio, const char *fname, size_t fnlen, int for_writing)
{
    filectx_t ctx = malloc(sizeof(struct filectx_s));
    fio_pathparts_t pp;

    if (ctx == 0) {
        return ctx;
	}
    memset(ctx, 0, sizeof(struct filectx_s));
    memset(&pp, 0, sizeof(pp));
    if (file_splitname(fio, fname, (int) fnlen, 1, &pp) == 0) {
        free(ctx);
        return 0;
    }
    ctx->fname = pp.path_fullname;
    ctx->fnlen = pp.path_fullnamelen;
    if (for_writing) {
        ctx->fd = open(ctx->fname, O_CREAT|O_TRUNC|O_WRONLY, 0666);
        ctx->is_output = 1;
    } else {
        ctx->fd = open(ctx->fname, O_RDONLY);
    }
    if (ctx->fd < 0) {
        char errbuf[64];
        errbuf[0] = '\0';
        strerror_r(errno, errbuf, sizeof(errbuf));
        log_signal(fio->logctx, 0, STC__OPENERR, ctx->fname, ctx->fnlen, errbuf);
        free(ctx->fname);
        free(ctx);
        return 0;
    }
    ctx->next = fio->open_files;
    fio->open_files = ctx;
    ctx->bufpos = 0;
    ctx->buflen = 0;
    ctx->fio = fio;
    return ctx;

} /* file_open */

filectx_t
file_open_input (fioctx_t fio, const char *fname, size_t fnlen) {
    return file_open(fio, fname, fnlen, 0);
}
filectx_t
file_open_output (fioctx_t fio, const char *fname, size_t fnlen) {
    return file_open(fio, fname, fnlen, 1);
}


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
    for (cur = fio->open_files; cur != 0; cur = cur->next) {
        if (cur == ctx)
            break;
        prev = cur;
    }
    if (cur == 0) {
        log_signal(fio->logctx, 0, STC__INTCMPERR, "file_close");
        return;
    }
    if (prev == 0) {
        fio->open_files = cur->next;
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

    if (ctx->is_output) {
        log_signal(ctx->fio->logctx, 0, STC__RDOUTFILE, ctx->fname);
        return -1;
    }

    while (bufsiz > 0) {
        if (remain > 0) {
            cp = memchr(bp, '\n', remain);
            if (cp != 0) {
                size_t count = cp - bp;
                if (count > bufsiz) {
                    log_signal(ctx->fio->logctx, 0, STC__LNTOOLONG,
                               "input", file_getname(ctx));
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
                log_signal(ctx->fio->logctx, 0, STC__LNTOOLONG,
                           "input", file_getname(ctx));
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

/*
 * file_writeline
 *
 * Returns:
 *       -1: error
 *      >-1: length of line written (without linemark)
 */
int
file_writeline (filectx_t ctx, const char *buf, size_t buflen)
{
    ssize_t n;
    struct iovec iov[2];

    iov[0].iov_base = (void *)buf;
    iov[0].iov_len = buflen;
    iov[1].iov_base = "\n";
    iov[1].iov_len = 1;
    n = writev(ctx->fd, iov, 2);
    if (n < 0) {
        char errbuf[64];
        errbuf[0] = '\0';
        strerror_r(errno, errbuf, sizeof(errbuf));
        log_signal(ctx->fio->logctx, 0, STC__FIOERR, errbuf);
        n = -1;
    }

    return (int)n - 1;

} /* file_writeline */
