/*
 *++
 * fileio.c - File input and output handling.
 *
 * This module implements all file I/O and path
 * name handling.
 *
 * The implementation here uses standard C I/O
 * calls, but abstracts the I/O handling such that
 * some OS-specific implementation could be used
 * if needed (e.g., for performance reasons).
 *
 * The readline/writline routines assume that files
 * are text divided into lines ending with linemarks
 * (for standard C I/O, '\n').
 *
 * Copyright © 2012-2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/uio.h>
#include "blissc/support/fileio.h"
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

/*
 * file_canonicalname
 *
 * Canonicalizes a name using realpath().  The file must exist
 * in the filesystem for this to work properly.
 */
char *
file_canonicalname (fioctx_t fio, const char *orig, int origlen, size_t *lenp)
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
    if (rpath != 0 && lenp != 0) *lenp = strlen(rpath);
    return rpath;

} /* file_canonicalname */

/*
 * file_splitname
 *
 * Parses a filename into its directory path, base name, and suffix.
 * If 'canonicalize' is non-zero, will canonicalize the filename before
 * parsing.  In either case, the fio_pathparts_t structure gets filled
 * in, and the file_fullname pointer in that structure will point to
 * a string buffer that the caller must free.
 */
int
file_splitname (fioctx_t fio, const char *orig, int origlen, int canonicalize,
                fio_pathparts_t *parts)
{
    size_t len, partlen;
    char *cp;

    memset(parts, 0, sizeof(fio_pathparts_t));
    if (canonicalize) {
        parts->path_fullname = file_canonicalname(fio, orig, origlen, &len);
        if (parts->path_fullname == 0) return 0;
        parts->path_fullnamelen = len;
    } else {
        size_t origlen_actual = (origlen < 0) ? strlen(orig) : (size_t) origlen;
        parts->path_fullname = malloc(origlen_actual+1);
        if (parts->path_fullname == 0) return 0;
        memcpy(parts->path_fullname, orig, origlen_actual);
        parts->path_fullname[origlen_actual] = '\0';
        len = parts->path_fullnamelen = origlen_actual;
    }
    for (cp = parts->path_fullname + (len-1), partlen = 0;
         cp >= parts->path_fullname && *cp != '.' && *cp != '/';
         cp -= 1, partlen += 1);
    if (cp >= parts->path_fullname) {
        if (*cp == '.') {
            parts->path_suffix = cp;
            parts->path_suffixlen = partlen+1;
            len = (size_t)(cp - parts->path_fullname);
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
        parts->path_absolute = (parts->path_dirnamelen > 0 &&
                                parts->path_dirname[0] == '/');
    } else {
        parts->path_filename = parts->path_fullname;
        parts->path_filenamelen = len;
    }

    return 1;

} /* file_splitname */

/*
 * file_combinename
 *
 * Creates a complete path for a filename from the directory,
 * basename, and suffix parts pointed to the 'parts' structure.
 * the path_fullname pointer in that structure will be freed if
 * non-NULL, with a new string buffer allocated, and filled in,
 * for the constructed name.  The caller must free the allocated
 * buffer.
 */
int
file_combinename (fioctx_t fio, fio_pathparts_t *parts)
{
    size_t len;
    char *newpath;
    newpath = malloc(parts->path_dirnamelen + parts->path_filenamelen +
                     parts->path_suffixlen + 1);
    if (newpath == 0) return 0;
    len = 0;
    if (parts->path_dirname != 0) {
        memcpy(newpath, parts->path_dirname, parts->path_dirnamelen);
        parts->path_dirname = newpath;
        len += parts->path_dirnamelen;
    }
    if (parts->path_filename != 0) {
        memcpy(newpath + len, parts->path_filename, parts->path_filenamelen);
        parts->path_filename = newpath+len;
        len += parts->path_filenamelen;
    }
    if (parts->path_suffix != 0) {
        memcpy(newpath + len, parts->path_suffix, parts->path_suffixlen);
        parts->path_suffix = newpath + len;
        len += parts->path_suffixlen;
    }
    if (parts->path_fullname != 0) free(parts->path_fullname);
    parts->path_fullname = newpath;
    parts->path_fullname[len] = '\0';
    parts->path_fullnamelen = len;
    return 1;

} /* file_combinename */

/*
 * file_freeparts
 *
 * Frees the string buffer allocated by one of the above
 * routines, and clears the contents of the 'parts'
 * structure.
 */
void
file_freeparts (fioctx_t fio, fio_pathparts_t *parts)
{
    if (parts->path_fullname != 0) {
        free(parts->path_fullname);
    }
    memset(parts, 0, sizeof(fio_pathparts_t));

} /* file_freeparts */

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
    if (file_splitname(fio, fname, (int) fnlen, (for_writing ? 0 : 1), &pp) == 0) {
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
 * flush_output_buffer
 *
 * Flushes the file buffer used for output files.
 */
static ssize_t
flush_output_buffer (filectx_t ctx)
{
    ssize_t n;

    n = write(ctx->fd, ctx->filebuf, ctx->buflen);
    if (n < 0) {
        char errbuf[64];
        errbuf[0] = '\0';
        strerror_r(errno, errbuf, sizeof(errbuf));
        log_signal(ctx->fio->logctx, 0, STC__FIOERR, errbuf);
        n = -1;
    }

    return n;

} /* flush_output_buffer */

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
    if (cur->is_output) {
        flush_output_buffer(cur);
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
 * -1: error occurred (errors are also signalled)
 *  0: end of file reached, no input
 * >0: success
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
 * file_readbuf
 *
 * Non-line-oriented file read.
 *
 * Returns:
 * -1: error occurred (errors are also signalled)
 *  0: end of file reached, no input
 * >0: success
 */
int
file_readbuf (filectx_t ctx, void *buf, size_t bufsiz, size_t *len)
{
    ssize_t ret;

    if (ctx->is_output) {
        log_signal(ctx->fio->logctx, 0, STC__RDOUTFILE, ctx->fname);
        return -1;
    }
    ret = read(ctx->fd, buf, bufsiz);
    if (ret < 0) {
        char errbuf[64];
        errbuf[0] = '\0';
        strerror_r(errno, errbuf, sizeof(errbuf));
        log_signal(ctx->fio->logctx, 0, STC__FIOERR, errbuf);
        return -1;
    }
    if (ret == 0) return 0;
    *len = ret;
    return 1;

} /* file_readbuf */

/*
 * file___write
 *
 * Internal write routine, used for both line-oriented
 * and raw writes.
 *
 * Returns:
 * -1: error
 * >-1: length of buffer written
 */
static ssize_t
file___write (filectx_t ctx, const void *buf, size_t buflen,
              int add_linemark)
{
    ssize_t n;

    if (buflen == 0 && !add_linemark) return 0;

    // Just append to the file buffer, if we can.
    if (ctx->buflen + buflen + add_linemark <= sizeof(ctx->filebuf)) {
        memcpy(ctx->filebuf + ctx->buflen, buf, buflen);
        ctx->buflen += buflen;
        if (add_linemark) ctx->filebuf[ctx->buflen++] = '\n';
        return (int) buflen;
    }
    // Too much data for the buffer.  Flush any pending buffered data first.
    if (ctx->buflen > 0) {
        if (flush_output_buffer(ctx) < 0) return -1;
    }
    // If the data fits in the buffer, put it there; otherwise, write it
    // directly.
    if (buflen + add_linemark < sizeof(ctx->filebuf)) {
        memcpy(ctx->filebuf, buf, buflen);
        ctx->buflen = buflen;
        if (add_linemark) ctx->filebuf[ctx->buflen++] = '\n';
    } else {
        struct iovec iov[2];
        iov[0].iov_base = (void *)buf;
        iov[0].iov_len = buflen;
        iov[1].iov_base = "\n";
        iov[1].iov_len = 1;
        n = writev(ctx->fd, iov, 1 + add_linemark);
        if (n < 0) {
            char errbuf[64];
            errbuf[0] = '\0';
            strerror_r(errno, errbuf, sizeof(errbuf));
            log_signal(ctx->fio->logctx, 0, STC__FIOERR, errbuf);
            return -1;
        }
    }
    return (int) buflen;

} /* file___write */

/*
 * file_writeline
 *
 * Returns:
 * -1: error
 * >-1: length of line written (without linemark)
 */
int
file_writeline (filectx_t ctx, const char *buf, size_t buflen)
{
    return (int) file___write(ctx, buf, buflen, 1);

} /* file_writeline */

/*
 * file_writebuf
 *
 * Non-line-oriented write.
 *
 * Returns:
 * -1: error
 * >-1: length of buffer written
 */
int
file_writebuf (filectx_t ctx, const void *buf, size_t buflen)
{
    return (int) file___write(ctx, buf, buflen, 0);

} /* file_writebuf */
