#ifndef strings_h__
#define strings_h__
/*
 *++
 * strings.h - string-handling definitions.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license.  See LICENSE.TXT for details.
 *--
 */
#include <string.h>

struct strctx_s;
typedef struct strctx_s *strctx_t;

/*
 * String descriptor structure.
 */
typedef struct {
    unsigned short flags;
#define STR_M_STATIC (1<<0)
    unsigned short len;
    char           *ptr;
} strdesc_t;

/*
 * Macros for static string descriptor setup
 * at declaration time.
 */
#define STRDEF(s) { STR_M_STATIC, sizeof(s)-1, s }
#define STRZDEF(s) { STR_M_STATIC, sizeof(s), s }

/*
 * Inline routine for initializing a static string
 * descriptor.
 */
static inline __attribute__((unused)) strdesc_t *
strdesc_init (strdesc_t *dsc, char *s, size_t len)
{
    dsc->flags = STR_M_STATIC;
    dsc->len   = len;
    dsc->ptr   = s;
    return dsc;
}

/*
 * STRING_MAXLEN is maximum length for a literal string that
 * the compiler will accept.
 */
#define STRING_MAXLEN 1024

strctx_t strings_init(void);
void strings_finish(strctx_t strctx);
strdesc_t *string_from_chrs(strctx_t strctx, strdesc_t *dest,
                            const char *cp, size_t len);
strdesc_t *ascic_string_from_chrs(strctx_t strctx, strdesc_t *dest,
                                  const char *cp, size_t len);
strdesc_t *string_append(strctx_t strctx, strdesc_t *trg, strdesc_t *add);
strdesc_t *string_alloc(strctx_t strctx, strdesc_t *dest, size_t len);
strdesc_t *string_copy(strctx_t strctx, strdesc_t *dest, strdesc_t *src);
void string_free(strctx_t strctx, strdesc_t *dsc);
int strings_eql(strdesc_t *a, strdesc_t *b);
strdesc_t *string_printf(strctx_t strctx, strdesc_t *dst, const char *fmt, ...);
int string_numval(strdesc_t *str, int base, long *valp);

#endif /* strings_h__ */
