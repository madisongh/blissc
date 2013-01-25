#ifndef listings_h__
#define listings_h__
/*
 *++
 *	File:			listings.h
 *
 *	Abstract:		Listings definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "nametable.h"
#include "support/logging.h"
#include <string.h>

struct lstgctx_s;
typedef struct lstgctx_s *lstgctx_t;

typedef enum {
    LISTOPT_SRC,
    LISTOPT_REQ,
    LISTOPT_EXP,
    LISTOPT_TRC,
    LISTOPT_LIB,
    LISTOPT_OBJ,
    LISTOPT_ASM,
    LISTOPT_SYM,
    LISTOPT_BIN,
    LISTOPT_COM
} listopt_type_t;
#define LISTOPT_COUNT (LISTOPT_COM+1)

lstgctx_t listings_init(scopectx_t kwdscope, logctx_t logctx);
void listings_finish(lstgctx_t ctx);
void listing_mainscope_set(lstgctx_t ctx, scopectx_t scope);
void listing_name_set(lstgctx_t ctx, strdesc_t *str);
void listing_title_set(lstgctx_t ctx, strdesc_t *str);
void listing_subtitle_set(lstgctx_t ctx, strdesc_t *str);
void listing_compilerid_set(lstgctx_t ctx, strdesc_t *str);
void listing_ident_set(lstgctx_t ctx, strdesc_t *str);
void listing_printline(void *, const char *, size_t, int);
int listing_printsrc(void *, char *, size_t, unsigned int, char);
int listing_open(lstgctx_t ctx, const char *fname, size_t len,
                 unsigned int flags);
void listing_file_close(void *);
void listing_require_begin(lstgctx_t ctx, char *fname, size_t fnlen);
void listing_newblock(lstgctx_t ctx);
void listing_endblock(lstgctx_t ctx, scopectx_t this_scope);

#endif /* listings_h__ */
