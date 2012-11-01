//
//  scanner.c
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "fileio.h"
#include "scanner.h"

#define SCAN_MAXFILES 16
#define SCAN_LINESIZE 1024

struct bufctx_s {
    filectx_t       fctx;
    char            linebuf[SCAN_LINESIZE];
    size_t          linelen;
    size_t          curpos;
    unsigned int    curline;
};

struct scanctx_s {
    struct bufctx_s bufstack[SCAN_MAXFILES];
    int             curbuf;
};

typedef enum {
    STATE_INIT,
    STATE_IN_EMBEDDED_COMMENT,
    STATE_IN_DECLIT,
    STATE_IN_QSTRING,
    STATE_IN_IDENTIFIER,
    STATE_ERRSKIP,
    STATE_EXIT
} scanstate_t;

const static char valid_ident_char[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, '$', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 0, 0, 0, 0, 0, 0,
    0, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 0, 0, 0, 0, '_',
    0, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

scanctx_t
scan_init (void)
{
    scanctx_t ctx = malloc(sizeof(struct scanctx_s));
    if (ctx != 0) {
        ctx->curbuf = -1;
    }
    return ctx;
}

void
scan_finish (scanctx_t ctx)
{
    int i;
    for (i = 0; i <= ctx->curbuf; i++) {
        file_close(ctx->bufstack[i].fctx);
    }
    free(ctx);
}

int
scan_fopen (scanctx_t ctx, const char *fname, size_t fnlen)
{
    int i = ctx->curbuf;
    if (i >= (SCAN_MAXFILES-1)) {
        /* XXX error condition */
        return 0;
    }
    i = i + 1;
    ctx->bufstack[i].fctx = file_open_input(fname, fnlen);
    if (ctx->bufstack[i].fctx == 0) {
        /* XXX error condition */
        return 0;
    }
    ctx->bufstack[i].curline = 0;
    ctx->bufstack[i].curpos = 0;
    ctx->bufstack[i].linelen = 0;
    ctx->curbuf = i;
    return 1;
}

scantype_t
scan_getnext (scanctx_t ctx, char *buf, size_t bufsiz, size_t *len,
              unsigned int flags)
{
    char *cp, *outp, ch;
    size_t remain;
    scanstate_t curstate = STATE_INIT;
    scantype_t rettype = SCANTYPE_END;

    outp = buf;
    while (ctx->curbuf >= 0) {
        struct bufctx_s *curbuf = &ctx->bufstack[ctx->curbuf];
        while (curbuf->curpos >= curbuf->linelen) {
            int rc;
            curbuf->curpos = 0;
            rc = file_readline(curbuf->fctx, curbuf->linebuf,
                                   sizeof(curbuf->linebuf), &curbuf->linelen);
            if (rc <= 0) {
                file_close(curbuf->fctx);
                ctx->curbuf -= 1;
                if (rc < 0) {
                    /* XXX error condition */
                    *len = outp - buf;
                    return SCANTYPE_ERR_FIO;
                }
                if (ctx->curbuf < 0) {
                    *len = outp - buf;
                    return SCANTYPE_END;
                }
                if ((flags & SCAN_M_ERRONEOF) != 0) {
                    /* XXX error condition */
                    *len = outp - buf;
                    return SCANTYPE_ERR_EOF;
                }
                curbuf = &ctx->bufstack[ctx->curbuf];
            } else {
                curbuf->curline += 1;
            }
        }

        cp = &curbuf->linebuf[curbuf->curpos];
        remain = curbuf->linelen - curbuf->curpos;
        while (remain > 0) {
            switch (curstate) {
                case STATE_IN_EMBEDDED_COMMENT:
                    if (*cp == ')') {
                        if (remain > 1 && *(cp+1) == '%') {
                            curstate = STATE_INIT;
                            cp += 1;
                            remain -= 1;
                        }
                    }
                    break;
                case STATE_IN_IDENTIFIER:
                    ch = valid_ident_char[*cp];
                    if (ch != 0) {
                        if (bufsiz > 0) {
                            *outp++ = ch;
                            bufsiz -= 1;
                        }
                    } else if (*cp == '%' &&
                               (remain > 1 && valid_ident_char[*(cp+1)])) {
                        rettype = SCANTYPE_ERR_INVID;
                        curstate = STATE_ERRSKIP;
                    } else {
                        rettype = SCANTYPE_IDENTIFIER;
                        curstate = STATE_EXIT;
                        cp -= 1;
                    }
                    break;
                case STATE_IN_DECLIT:
                    if (isdigit(*cp)) {
                        if (bufsiz > 0) {
                            *outp++ = *cp;
                            bufsiz -= 1;
                        }
                    } else if (valid_ident_char[*cp] ||
                        (remain > 1 && *cp == '%' && valid_ident_char[*(cp+1)])) {
                        rettype = SCANTYPE_ERR_INVLIT;
                        curstate = STATE_ERRSKIP;
                    } else {
                        rettype = SCANTYPE_DECLITERAL;
                        curstate = STATE_EXIT;
                        cp -= 1;
                    }
                    break;
                case STATE_IN_QSTRING:
                    if (*cp == '\'') {
                        if (remain > 1 && *(cp+1) == '\'') {
                            if (bufsiz > 0) {
                                *outp++ = '\'';
                                bufsiz -= 1;
                            }
                        } else {
                            rettype = SCANTYPE_QUOTEDSTRING;
                            curstate = STATE_EXIT;
                        }
                    } else if (bufsiz > 0) {
                        *outp++ = *cp;
                        bufsiz -= 1;
                    }
                    break;
                case STATE_INIT:
                    if (*cp == ' ' || *cp == '\t' || *cp == '\013' || *cp == '\014') {
                        break;
                    }
                    if (*cp == '!') {
                        cp += remain-1; // advance to end of line
                        remain = 1;
                        break;
                    }
                    if (*cp == '%' && remain > 1 && *(cp+1) == '(') {
                        cp += 1;
                        remain -= 1;
                        curstate = STATE_IN_EMBEDDED_COMMENT;
                        break;
                    }
                    if (*cp == '\'') {
                        curstate = STATE_IN_QSTRING;
                        break;
                    }
                    if (isdigit(*cp)) {
                        if (bufsiz > 0) {
                            *outp++ = *cp;
                            bufsiz -= 1;
                        }
                        curstate = STATE_IN_DECLIT;
                        break;
                    }
                    if (valid_ident_char[*cp] ||
                        (*cp == '%' && remain > 1 && valid_ident_char[*(cp+1)])) {
                        if (bufsiz > 0) {
                            *outp++ = (*cp == '%' ? '%' : valid_ident_char[*cp]);
                            bufsiz -= 1;
                        }
                        curstate = STATE_IN_IDENTIFIER;
                        break;
                    }
                    if (strchr(".^*/+-=", *cp) != 0) {
                        if (bufsiz > 0) {
                            *outp++ = *cp;
                            bufsiz -= 1;
                        }
                        rettype = SCANTYPE_OPERATOR;
                        curstate = STATE_EXIT;
                        break;
                    }
                    // This check treats % as punctuation, so it must
                    // follow any special-case checks (start of identifier
                    // or start of embedded comment).
                    if (strchr(",;:()[]<>%", *cp) != 0) {
                        if (bufsiz > 0) {
                            *outp++ = *cp;
                            bufsiz -= 1;
                        }
                        rettype = SCANTYPE_PUNCTUATION;
                        curstate = STATE_EXIT;
                        break;
                    }
                    // unrecognized character
                    rettype = SCANTYPE_ERR_INVCHR;
                    curstate = STATE_ERRSKIP;
                    break;
                case STATE_ERRSKIP:
                    // hit an error condition; try skipping ahead
                    // to the next available whitespace or end of line
                    // this logic could probably be made more intelligent
                    cp++; remain--;
                    while (remain > 0 &&
                           !(*cp == ' ' || *cp == '\t' || *cp == '\013' || *cp == '\014')) {
                        cp++;
                        remain--;
                    }
                    curbuf->curpos = cp - &curbuf->linebuf[0];
                    *len = outp - buf;
                    return rettype;
                case STATE_EXIT:
                    // normal exit state, expects rettype to be
                    // set correctly before entry
                    curbuf->curpos = cp - &curbuf->linebuf[0];
                    *len = outp - buf;
                    return rettype;
            } /* switch */

            cp += 1;
            remain -= 1;

        } /* while remain > 0 */

        // OK, hit end of line (linemark).

        curbuf->curpos = cp - &curbuf->linebuf[0];
        switch (curstate) {
            case STATE_IN_DECLIT:
                *len = outp - buf;
                return SCANTYPE_DECLITERAL;
            case STATE_IN_IDENTIFIER:
                *len = outp - buf;
                return SCANTYPE_IDENTIFIER;
            case STATE_IN_QSTRING:
                return SCANTYPE_ERR_QSTR;
            case STATE_EXIT:
                *len = outp - buf;
                return rettype;
            default:
                break;
        }


    } /* while ctx->curbuf >= 0 */

    *len = outp - buf;
    return rettype;
}