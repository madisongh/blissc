//
//  fileio.c
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "fileio.h"

struct filectx_s {
    struct filectx_s *next;
    int               fd;
    size_t            fnlen;
    size_t            bufpos;
    size_t            buflen;
    char              *fname;
    char              filebuf[4096];
};

static filectx_t input_files;

/*
 * Note that file names are NOT freed with the
 * file context structures; this is on purpose,
 * as we may need to display the file name in
 * an error message after it is closed.
 */

void
fileio_init (void)
{
    input_files = 0;
}

void
fileio_finish (void)
{
    filectx_t ctx;
    for (ctx = input_files; ctx != 0; ctx = input_files) {
        input_files = ctx->next;
        close(ctx->fd);
        free(ctx);
    }
}

filectx_t
file_open_input (const char *fname, size_t fnlen)
{
    filectx_t ctx = malloc(sizeof(struct filectx_s));
    if (ctx == 0)
        return ctx;
    ctx->fname = malloc(fnlen+1);
    if (ctx->fname == 0) {
        free(ctx);
        return 0;
    }
    memcpy(ctx->fname, fname, fnlen);
    ctx->fname[fnlen] = '\0';
    ctx->fnlen = fnlen;
    ctx->fd = open(ctx->fname, O_RDONLY);
    if (ctx->fd < 0) {
        /* XXX error condition */
        free(ctx);
        return 0;
    }
    ctx->next = input_files;
    input_files = ctx;
    ctx->bufpos = 0;
    ctx->buflen = 0;
    return ctx;
}

void
file_close (filectx_t ctx)
{
    filectx_t prev, cur;
    prev = 0;
    for (cur = input_files; cur != 0; cur = cur->next) {
        if (cur == ctx)
            break;
        prev = cur;
    }
    if (cur == 0) {
        /* XXX error condition */
        return;
    }
    if (prev == 0) {
        input_files = cur->next;
    } else {
        prev->next = cur->next;
    }
    close(cur->fd);
    free(cur);
}

char *
file_getname (filectx_t ctx)
{
    return ctx->fname;
}

int
file_readline (filectx_t ctx, char *buf, size_t bufsiz, size_t *len)
{
    char *outp = buf;
    char *origbp = &ctx->filebuf[ctx->bufpos];
    char *bp = origbp;
    char *cp;
    size_t remain = ctx->buflen - ctx->bufpos;
    ssize_t ret;
    int status;

    while (bufsiz > 0) {
        if (remain > 0) {
            cp = memchr(bp, '\n', remain);
            if (cp != 0) {
                size_t count = cp - bp;
                if (count > bufsiz) {
                    /* XXX error condition */
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
                /* XXX error condition */
                status = -1;
                break;
            }
            memcpy(outp, bp, remain);
            outp += remain;
            bufsiz -= remain;
        }
        ret = read(ctx->fd, ctx->filebuf, sizeof(ctx->filebuf));
        if (ret < 0) {
            /* XXX error condition */
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

}
