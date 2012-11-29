//
//  strings.c
//  blissc
//
//  Created by Matthew Madison on 10/31/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "strings.h"

#define POOL_NONE   (-1)
#define POOL_SMALL  0
#define POOL_MEDIUM 1
#define POOL_LARGE  2
#define POOL_COUNT  (POOL_LARGE+1)

#define SMALLSZ     16
#define MEDSZ       64
#define LRGSZ       STRING_MAXLEN

#define ALLOCOUNT   256

struct string_s {
    struct string_s *next;
    int              poolindex;
    strdesc_t        desc;
};
typedef struct string_s string_t;

struct stringpool_s {
    string_t        *freelist;
    size_t           maxsize;
};

static struct stringpool_s  pool[POOL_COUNT] = {
    { 0, SMALLSZ }, { 0, MEDSZ }, { 0, LRGSZ }
};

static inline int
is_static (strdesc_t *dsc)
{
    return ((dsc->flags & STR_M_STATIC) != 0);
}

static inline int
size_to_pool (size_t len)
{
    int i;
    for (i = 0; i < POOL_COUNT; i++) {
        if (len <= pool[i].maxsize) {
            return i;
        }
    }
    return POOL_NONE;
}

static inline string_t *
desc_to_str (strdesc_t *dsc)
{
    return is_static(dsc) ? 0 : (string_t *)(dsc->ptr - sizeof(string_t));
}
static int
stringpool_expand (int idx)
{
    string_t *more = malloc((pool[idx].maxsize + sizeof(string_t))*ALLOCOUNT);
    string_t *str;
    char *cp;
    int i;

    if (more == 0) {
        return 0;
    }

    for (cp = (char *)more, i = 0; i < ALLOCOUNT;
         i++, cp += pool[idx].maxsize) {
        str = (string_t *) cp;
        cp += sizeof(string_t);
        str->poolindex = idx;
        str->next = (string_t *)(cp + pool[idx].maxsize);
        str->desc.flags = 0;
        str->desc.len = 0;
        str->desc.ptr = cp;
    }
    // on exit, 'str' still points to the last allocated entry
    str->next = pool[idx].freelist;
    pool[idx].freelist = (string_t *) more;
    return 1;
}

int
stringpool_init (void)
{
    int i;

    for (i = 0; i < POOL_COUNT; i++) {
        if (!stringpool_expand(i)) {
            return 0;
        }
    }
    return 1;
}

static string_t *
string___alloc (size_t len)
{
    string_t *str;
    int i = size_to_pool(len);

    if (i == POOL_NONE) {
        return 0;
    }
    if (pool[i].freelist == 0) {
        if (!stringpool_expand(i)) {
            return 0;
        }
    }
    str = pool[i].freelist;
    pool[i].freelist = str->next;
    str->next = 0;
    return str;
}

static void
string___free (string_t *str)
{
    if (str->poolindex == POOL_NONE) {
        return;
    }
    str->next = pool[str->poolindex].freelist;
    pool[str->poolindex].freelist = str;
}

strdesc_t *
string_from_chrs (strdesc_t *dest, const char *cp, size_t len)
{
    string_t *str = string___alloc(len);

    if (str == 0) {
        return 0;
    }
    if (dest == 0) {
        dest = &str->desc;
    } else {
        dest->ptr = str->desc.ptr;
        dest->flags = 0;
    }
    dest->len = len;
    memcpy(dest->ptr, cp, len);
    return dest;
}

strdesc_t *
ascic_string_from_chrs (strdesc_t *dest, const char *cp, size_t len)
{
    string_t *str = string___alloc(len+1);
    if (str == 0) {
        return 0;
    }
    if (dest == 0) {
        dest = &str->desc;
    } else {
        dest->ptr = str->desc.ptr;
        dest->flags = 0;
    }
    dest->len = len+1;
    memcpy(dest->ptr+1, cp, len);
    *(dest->ptr) = (len & 0xff);
    return dest;
}

strdesc_t *
string_append (strdesc_t *trg, strdesc_t *add)
{
    string_t *str;
    string_t *newstr;

    if (trg == 0) {
        return string_copy(trg, add);
    }
    str = desc_to_str(trg);
    if (is_static(trg) ||
        (trg->len + add->len <= pool[str->poolindex].maxsize)) {
        memcpy(trg->ptr + trg->len, add->ptr, add->len);
        trg->len += add->len;
        return trg;
    }
    newstr = string___alloc(trg->len + add->len);
    if (newstr == 0) {
        return 0;
    }
    newstr->desc.len = trg->len + add->len;
    memcpy(newstr->desc.ptr, trg->ptr, trg->len);
    memcpy(newstr->desc.ptr + trg->len, add->ptr, add->len);
    string___free(str);
    return &newstr->desc;
}

void
string_free (strdesc_t *dsc)
{
    if (is_static(dsc)) {
        return;
    }
    string___free(desc_to_str(dsc));
}

strdesc_t *
string_alloc (strdesc_t *dest, size_t len)
{
    string_t *str = string___alloc(len);
    if (str == 0) {
        return 0;
    }
    if (dest == 0) {
        dest = &str->desc;
    }
    dest->flags = 0;
    dest->ptr = str->desc.ptr;
    dest->len = len;
    return dest;
}

strdesc_t *
string_copy (strdesc_t *dest, strdesc_t *src)
{
    string_t *str;

    if (dest != 0 && !is_static(dest)) {
        str = desc_to_str(dest);
        if (pool[str->poolindex].maxsize >= src->len) {
            dest->len = src->len;
            memcpy(dest->ptr, src->ptr, src->len);
            return dest;
        }
        string___free(str);
    }
    str = string___alloc(src->len);
    if (str == 0) {
        return 0;
    }
    if (dest == 0) {
        dest = &str->desc;
    } else {
        dest->ptr = str->desc.ptr;
    }
    dest->len = src->len;
    memcpy(dest->ptr, src->ptr, src->len);
    return dest;
}

int
strings_eql (strdesc_t *a, strdesc_t *b)
{
    if (a->len != b->len) {
        return 0;
    }
    return memcmp(a->ptr, b->ptr, a->len) == 0;
}

strdesc_t *
string_printf (strdesc_t *dst, const char *fmt, ...)
{
    va_list ap;
    char buf[256];
    int len;

    va_start(ap, fmt);
    len = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    return string_from_chrs(dst, buf, (len < 0 ? 0 : len));

} /* string_printf */

int
string_numval (strdesc_t *str, int base, long *valp)
{
    char buf[32], *cp;
    long numval;

    if (str->len >= sizeof(buf)) {
        return 0;
    }
    memcpy(buf, str->ptr, str->len);
    buf[str->len] = '\0';
    errno = 0;
    numval = strtol(buf, &cp, base);
    if (errno != 0 || (cp-buf) != str->len) {
        return 0;
    }
    *valp = numval;
    return 1;
}