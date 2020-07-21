/*
 *++
 * scanner.c - Lexeme scanner.
 *
 * This module is responsible for scanning an external
 * input stream (a file or programmed input source) and
 * breaking the input up into tokens that represent BLISS
 * lexemes.  It is the lowest layer of the lexical processing
 * subsystem.
 *
 * The scanner can be directed to point either files or
 * "programmed" input sources, where the caller provides
 * a callback function for fetching the next line of input.
 * The callback function is expected to return the length of
 * the input line, zero for EOF, or a negative return code
 * for an error, just like the file I/O routines do.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "blissc/scanner.h"
#include "blissc/support/fileio.h"

#define SCAN_MAXFILES 16
#define SCAN_LINESIZE 1024

// Structure for tracking an input stream and
// the current line of text being scanned.
struct streamctx_s {
    struct streamctx_s *next;
    struct scanctx_s   *scanctx;
    filectx_t           fctx;
    scan_input_fn       inpfn;
    void               *fnctx;
    size_t              linelen;
    size_t              curpos;
    unsigned int        curline;
    strdesc_t           linedsc;
    char                linebuf[SCAN_LINESIZE];
    char                tokbuf[SCAN_LINESIZE];
};

// Module context structure.
struct scanctx_s {
    strctx_t        strctx;
    logctx_t        logctx;
    fioctx_t        fioctx;
    void           *lstctx;
    scan_list_fn    listfn;
    scan_close_fn   closefn;
    streamctx_t     freestreams;
};

// States for the scanner's state machine.
typedef enum {
    STATE_INIT,
    STATE_IN_EMBEDDED_COMMENT,
    STATE_IN_DECLIT,
    STATE_IN_QSTRING,
    STATE_IN_IDENTIFIER,
    STATE_ERRSKIP,
    STATE_EXIT
} scanstate_t;

// Lookup table for identifying characters that are
// valid for BLISS names/keywords.
static const char valid_ident_charmap[256] = {
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
static inline char valid_ident_char(char c) { return valid_ident_charmap[(int)c]; }

/*
 * Memory management
 *
 */
static streamctx_t
stream_alloc (scanctx_t ctx)
{
    streamctx_t s;

    if (ctx->freestreams == 0) {
        s = malloc(sizeof(struct streamctx_s));
    } else {
        s = ctx->freestreams;
        ctx->freestreams = s->next;
    }
    if (s != 0) {
        memset(s, 0, sizeof(struct streamctx_s));
        s->scanctx = ctx;
    }
    return s;

} /* stream_alloc */

static void
stream_free (streamctx_t s)
{
    if (s == 0) return;
    s->next = s->scanctx->freestreams;
    s->scanctx->freestreams = s;

} /* stream_free */

/*
 * scan_init
 *
 * Initializes the scanner.
 */
scanctx_t
scan_init (strctx_t strctx, logctx_t logctx, void *fioctx)
{
    scanctx_t ctx = malloc(sizeof(struct scanctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct scanctx_s));
        ctx->strctx = strctx;
        ctx->logctx = logctx;
        ctx->fioctx = fioctx;
    }
    return ctx;

} /* scan_init */

void scan_listfuncs_set (scanctx_t ctx, scan_list_fn lf, scan_close_fn cf, void *lstctx) {
    ctx->listfn = lf; ctx->closefn = cf; ctx->lstctx = lstctx;
}

/*
 * scan_finish
 *
 * Shuts down the scanner.
 */
void
scan_finish (scanctx_t ctx)
{
    streamctx_t s, snext;
    for (s = ctx->freestreams; s != 0; s = snext) {
        snext = s->next;
        free(s);
    }
    free(ctx);
}

/*
 * scan_fopen
 *
 * Opens a file and uses it for the current input
 * stream.  Any currently-open input stream will be
 * pushed on the buffer stack, and will be returned to
 * after the new input stream reaches its end.
 */
streamctx_t
scan_fopen (scanctx_t ctx, const char *fname, size_t fnlen,
            char **actnamep)
{
    streamctx_t s = stream_alloc(ctx);
    if (s == 0) {
        return 0;
    }
    s->fctx = file_open_input(ctx->fioctx, fname, fnlen);
    if (s->fctx == 0) {
        stream_free(s);
        return 0;
    }
    if (actnamep) *actnamep = file_getname(s->fctx);
    return s;

} /* scan_fopen */

/*
 * scan_popen
 *
 * Opens a programmed input stream - one for which the caller
 * provides a callback function to obtain the next line of
 * input.
 */
streamctx_t
scan_popen (scanctx_t ctx, scan_input_fn infn, void *fnctx)
{
    streamctx_t s = stream_alloc(ctx);
    if (s == 0) {
        return 0;
    }
    s->inpfn = infn;
    s->fnctx = fnctx;
    return s;

} /* scan_popen */

/*
 * scan_close
 *
 * Closes an open stream.
 */
void
scan_close (streamctx_t s)
{
    scanctx_t ctx;
    if (s == 0) return;
    ctx = s->scanctx;
    if (s->fctx) {
        if (ctx->closefn != 0) {
            (*ctx->closefn)(ctx->lstctx);
        }
        file_close(s->fctx);
    }
    stream_free(s);

} /* scan_close */

/*
 * scan_getnext
 *
 * Gets the next token from the current input stream.  The
 * caller can set the SCAN_M_ERRONEOF flag if reaching the end
 * of file on the current input source should be treated as
 * an error - the BLISS LRM identifies a small number of situations
 * when that should be the case.
 * The other flag that can be passed in is SCAN_M_SIGNOK, which
 * the caller sets when it is OK for a +/- sign to be treated as
 * a lead-in to a numeric literal (which is not universal, per the
 * LRM).
 */
scantype_t
scan_getnext (streamctx_t strm, unsigned int flags, strdesc_t **tok,
              unsigned int *lineno, unsigned int *column)
{
    scanctx_t ctx = strm->scanctx;
    char *cp, *outp, ch;
    size_t remain, bufsiz, len;
    scanstate_t curstate = STATE_INIT;
    scantype_t rettype = SCANTYPE_END;

    outp = strm->tokbuf;
    bufsiz = sizeof(strm->tokbuf);
    while (1) {
        while (strm->curpos >= strm->linelen) {
            int rc;
            strm->curpos = 0;
            if (strm->inpfn != 0) {
                rc = (strm->inpfn)(strm->fnctx, strm->linebuf,
                                   sizeof(strm->linebuf), &strm->linelen);
            } else {
                rc = file_readline(strm->fctx, strm->linebuf,
                                   sizeof(strm->linebuf), &strm->linelen);
            }
            if (rc < 0) {
                log_signal(ctx->logctx, 0, STC__FIOERR,
                           (strm->inpfn == 0 ? file_getname(strm->fctx)
                            : "(internal stream)"));
                len = outp - strm->tokbuf;
                *tok = string_from_chrs(ctx->strctx, 0, strm->tokbuf, len);
                *lineno = strm->curline;
                *column = 0;
                return SCANTYPE_ERR_FIO;
            } else if (rc == 0) {
                if ((flags & SCAN_M_ERRONEOF) != 0) {
                    log_signal(ctx->logctx, 0, STC__EOFERR,
                               (strm->inpfn == 0 ? file_getname(strm->fctx)
                                : "(internal stream)"));
                }
                len = outp - strm->tokbuf;
                *tok = string_from_chrs(ctx->strctx, 0, strm->tokbuf, len);
                *lineno = strm->curline;
                *column = 0;
                return (flags & SCAN_M_ERRONEOF) ? SCANTYPE_ERR_EOF : SCANTYPE_END;
            } else {
                strm->curline += 1;
                if (ctx->listfn != 0) {
                    (*ctx->listfn)(ctx->lstctx, strm->linebuf, strm->linelen, strm->curline,
                                   (curstate == STATE_IN_EMBEDDED_COMMENT ? 'C' : ' '));
                }
            }

        } /* while needing to fetch another line */

        cp = &strm->linebuf[strm->curpos];
        remain = strm->linelen - strm->curpos;
        while (remain > 0) {
            switch (curstate) {
                case STATE_INIT:
                    // skip whitespace
                    if (*cp == ' ' || *cp == '\t' || *cp == '\013' || *cp == '\014') {
                        break;
                    }
                    // comment (through end of line)
                    if (*cp == '!') {
                        cp += remain-1; // advance to end of line
                        remain = 1;
                        break;
                    }
                    // %(...)% 'embedded' comment
                    if (*cp == '%' && remain > 1 && *(cp+1) == '(') {
                        cp += 1;
                        remain -= 1;
                        curstate = STATE_IN_EMBEDDED_COMMENT;
                        break;
                    }
                    // If we get here, we know we're staring a
                    // real token, so record its starting position
                    *lineno = strm->curline;
                    *column = (unsigned int)(cp - &strm->linebuf[0]);
                    // Apostrophe introduces a quoted-string literal
                    if (*cp == '\'') {
                        curstate = STATE_IN_QSTRING;
                        break;
                    }
                    // Digit or sign (if SINGOK is set) introduces a
                    // decimal numeric literal
                    if (isdigit(*cp) ||
                        ((flags & SCAN_M_SIGNOK) &&
                         (*cp == '+' || *cp == '-') && isdigit(*(cp+1)))) {
                            if (bufsiz > 0) {
                                *outp++ = *cp;
                                bufsiz -= 1;
                            }
                            curstate = STATE_IN_DECLIT;
                            break;
                        }
                    // See if we have something that looks like a name.
                    // The '%' character can be used in a name (typically
                    // as the first character), but only if it is immediately
                    // followed by a normal identifier-type character.
                    if (valid_ident_char(*cp) ||
                        (*cp == '%' && remain > 1 && valid_ident_char(*(cp+1)))) {
                        if (bufsiz > 0) {
                            *outp++ = (*cp == '%' ? '%' : valid_ident_char(*cp));
                            bufsiz -= 1;
                        }
                        curstate = STATE_IN_IDENTIFIER;
                        break;
                    }
                    // These are operator characters.  Note that the
                    // + and - characters are included here; we already
                    // checked above for their alternative role as part
                    // of a decimal numeric literal.
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
                    ch = valid_ident_char(*cp);
                    if (ch != 0) {
                        if (bufsiz > 0) {
                            *outp++ = ch;
                            bufsiz -= 1;
                        }
                    } else if (*cp == '%' &&
                               (remain > 1 && valid_ident_char(*(cp+1)))) {
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
                    } else if (valid_ident_char(*cp) ||
                               (remain > 1 && *cp == '%' && valid_ident_char(*(cp+1)))) {
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
                                cp++; remain--;
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
                    strm->curpos = cp - &strm->linebuf[0];
                    len = outp - strm->tokbuf;
                    *tok = string_from_chrs(ctx->strctx, 0, strm->tokbuf, len);
                    return rettype;

                case STATE_EXIT:
                    // normal exit state, expects rettype to be
                    // set correctly before entry
                    strm->curpos = cp - &strm->linebuf[0];
                    len = outp - strm->tokbuf;
                    *tok = string_from_chrs(ctx->strctx, 0, strm->tokbuf, len);
                    return rettype;

            } /* switch */

            cp += 1;
            remain -= 1;

        } /* while remain > 0 */
        
        // OK, hit end of line (linemark), possibly before
        // identifying a complete token.  Complete that work
        // here.
        
        strm->curpos = cp - &strm->linebuf[0];
        switch (curstate) {
            case STATE_IN_DECLIT:
                rettype = SCANTYPE_DECLITERAL;
                goto depart;
            case STATE_IN_IDENTIFIER:
                rettype = SCANTYPE_IDENTIFIER;
                goto depart;
            case STATE_IN_QSTRING:
                return SCANTYPE_ERR_QSTR;
            case STATE_EXIT:
                goto depart;
            default:
                break;
        }
    }

depart:
    len = outp - strm->tokbuf;
    *tok = string_from_chrs(ctx->strctx, 0, strm->tokbuf, len);
    return rettype;

} /* scan_getnext */

/*
 * scan_curline_get
 *
 * Returns the current line.  Use for error messages.
 */
strdesc_t *
scan_curline_get (streamctx_t strm)
{
    if (strm == 0) return 0;
    strdesc_init(&strm->linedsc, strm->linebuf, strm->linelen);
    return &strm->linedsc;

} /* scan_curline_get */
