#ifndef scanner_h__
#define scanner_h__
/*
 *++
 *	File:			scanner.h
 *
 *	Abstract:		Scanner definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "logging.h"
#include "strings.h"

struct scanctx_s;
typedef struct scanctx_s *scanctx_t;
struct streamctx_s;
typedef struct streamctx_s *streamctx_t;

typedef enum {
    SCANTYPE_IDENTIFIER,
    SCANTYPE_DECLITERAL,
    SCANTYPE_QUOTEDSTRING,
    SCANTYPE_OPERATOR,
    SCANTYPE_PUNCTUATION,
    SCANTYPE_END,
    SCANTYPE_ERR_EOF,
    SCANTYPE_ERR_FIO,
    SCANTYPE_ERR_INVLIT,
    SCANTYPE_ERR_INVID,
    SCANTYPE_ERR_QSTR,
    SCANTYPE_ERR_INVCHR
} scantype_t;

#define SCAN_M_ERRONEOF (1<<0)
#define SCAN_M_SIGNOK   (1<<1)

static inline int __unused scan_ok (scantype_t typ) {
    return (typ < SCANTYPE_ERR_EOF);
}

typedef int (*scan_input_fn)(void *ctx, char *buf, size_t siz, size_t *len);
typedef int (*scan_list_fn)(void *ctx, char *buf, size_t len,
                            unsigned int lineno, char lexcode);
typedef void (*scan_close_fn)(void *ctx);

scanctx_t scan_init(strctx_t strctx, logctx_t logctx, void *fioctx);
void scan_listfuncs_set(scanctx_t sctx, scan_list_fn lfunc,
                        scan_close_fn cfunc, void *ctx);
streamctx_t scan_fopen(scanctx_t ctx, const char *fname, size_t fnlen,
               const char *suffix, char **actnamep);
streamctx_t scan_popen(scanctx_t ctx, scan_input_fn infn, void *fnctx);
scantype_t scan_getnext(streamctx_t ctx, unsigned int flags,
                        strdesc_t **tok, unsigned int *lineno,
                        unsigned int *column);
strdesc_t *scan_curline_get(streamctx_t ctx);
void scan_close(streamctx_t ctx);
void scan_finish(scanctx_t ctx);

#endif /* scanner_h__ */
