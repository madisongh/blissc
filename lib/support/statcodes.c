/*
 *++
 * statcodes.c - Status codes and messages
 *
 * This module implements the message formatting
 * parts of the status code facility.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#define STATCODES_INSTANTIATE
#include "blissc/support/statcodes.h"
#include "blissc/support/strings.h"

#define STATCODE(msg,typ,nam,txt) #nam,
static const char *mcnames[] = {
    STATCODES
};
#undef STATCODE

#define STATCODE(msg,typ,nam,txt) txt,
static const char *mctext[] = {
    STATCODES
};
#undef STATCODE

#define MSG_COUNT (sizeof(mctext)/sizeof(mctext[0]))
static const char sevchars[8] = "?SI??WEF";

/*
 * stc_msg_format
 *
 * Message formatting, with variable-length arg list.
 */
int
stc_msg_format (statcode_t statcode, char *buf, size_t bufsiz, ...)
{
    va_list ap;
    int result;

    va_start(ap, bufsiz);
    result = stc_msg_vformat(statcode, buf, bufsiz, ap);
    va_end(ap);
    return result;

} /* stc_msg_format */

/*
 * stc_msg_vformat
 *
 * The core formatting routine.
 * Do NOT use sprintf-style formatting codes in
 * message strings! Use the formatting directives
 * below:
 * !SL = text with length (char *str, int len)
 * !SZ = null-terminated string (char *azstr)
 * !SD = string descriptor (strdesc_t *sdsc)
 * !U{I,S,L} = unsigned int, short, long
 * !I{I,S,L} = signed int, short, long
 * !X{I,S,L} = hexadecimal int, short, long
 * !P = pointer
 * !! = exclamation point
 */
int
stc_msg_vformat (statcode_t statcode, char *buf, size_t bufsiz, va_list ap)
{
    int len;
    char tmpbuf[32];
    char *outp;
    const char *fmt;
    unsigned int msgno = stc_msgno(statcode);
    unsigned int sev = stc_severity(statcode);

    if (msgno >= MSG_COUNT) {
        return snprintf(buf, bufsiz, "%%BLISS-%c-UNKNOWN: unknown message code %u",
                       sevchars[sev], msgno);
    }

    len = snprintf(buf, bufsiz, "%%BLISS-%c-%s, ", sevchars[sev], mcnames[msgno]);
    if (len >= bufsiz) len = (int) bufsiz - 1;
    outp = buf + len;
    bufsiz -= len;

    fmt = mctext[msgno];

    while (bufsiz > 0 && *fmt != '\0') {
        if (*fmt != '!') {
            *outp++ = *fmt++;
            bufsiz -= 1;
            continue;
        }
        fmt += 1;
        if (*fmt == '\0') break;
        if (*fmt == '!') {
            *outp++ = *fmt++;
            bufsiz -= 1;
            continue;
        }
        if (*fmt == 'U' || *fmt == 'X') {
            unsigned long val;
            if (*(fmt+1) == 'L') {
                val = va_arg(ap, unsigned long);
            } else if (*(fmt+1) == 'S' || *(fmt+1) == 'I') {
                val = va_arg(ap, unsigned int);
            } else continue;
            len = snprintf(tmpbuf, sizeof(tmpbuf),
                           (*fmt == 'U' ? "%lu" : "%lX"), val);
            if (len < 0) { len = 0; }
            if (len >= bufsiz) { len = (int) bufsiz-1; }
            memcpy(outp, tmpbuf, len);
            outp += len;
            bufsiz -= len;
            fmt += 2;
            continue;
        }
        if (*fmt == 'I') {
            long val;
            if (*(fmt+1) == 'L') {
                val = va_arg(ap, long);
            } else if (*(fmt+1) == 'S' || *(fmt+1) == 'I') {
                val = va_arg(ap, int);
            } else continue;
            len = snprintf(tmpbuf, sizeof(tmpbuf), "%ld", val);
            if (len < 0) { len = 0; }
            if (len >= bufsiz) { len = (int) bufsiz-1; }
            memcpy(outp, tmpbuf, len);
            outp += len;
            bufsiz -= len;
            fmt += 2;
            continue;
        }
        if (*fmt == 'P') {
            len = snprintf(tmpbuf, sizeof(tmpbuf), "%p", va_arg(ap, void *));
            if (len < 0) { len = 0; }
            if (len >= bufsiz) { len = (int) bufsiz-1; }
            memcpy(outp, tmpbuf, len);
            outp += len;
            bufsiz -= len;
            fmt += 1;
            continue;
        }
        if (*fmt == 'S') {
            unsigned int slen;
            char *ptr;
            if (*(fmt+1) == 'L') {
                ptr = va_arg(ap, char *);
                slen = va_arg(ap, unsigned int);
            } else if (*(fmt+1) == 'Z') {
                ptr = va_arg(ap, char *);
                slen = (unsigned int)strlen(ptr);
            } else if (*(fmt+1) == 'D') {
                strdesc_t *dsc = va_arg(ap, strdesc_t *);
                ptr = dsc->ptr;
                slen = dsc->len;
            } else continue;
            if (slen >= (unsigned int) bufsiz) slen = (unsigned int)bufsiz-1;
            memcpy(outp, ptr, slen);
            outp += slen;
            bufsiz -= slen;
            fmt += 2;
            continue;
        }
        // Unrecognized directive here, just loop back up to the top
    } /* while bufsiz > 0 && *fmt != null char */

    return (int)(outp - buf);

} /* stc_msg_vformat */
