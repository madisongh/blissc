//
//  scanner.h
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_scanner_h
#define blissc_scanner_h

#include "strings.h"

struct scanctx_s;
typedef struct scanctx_s *scanctx_t;

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

scanctx_t scan_init(void);
int scan_fopen(scanctx_t ctx, const char *fname, size_t fnlen);
scantype_t scan_getnext(scanctx_t ctx, unsigned int flags, strdesc_t **tok);
void scan_finish(scanctx_t ctx);

#endif
