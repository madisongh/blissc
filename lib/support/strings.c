/*
 *++
 *	File:			strings.c
 *
 *	Abstract:		Dynamic string management.
 *
 *  Module description:
 *		This module contains all of the memory management
 *		for dynamic strings, along with functions for manipulating
 *		strings via string descriptors.
 *
 *		Note that descriptors are embedded in each string cell,
 *		and the descriptor-based API will return pointers to
 *		dynamically-allocated descriptors.  However, callers
 *		can create their own descriptors, if it's more
 *		convenient to do so, and statically-allocated strings
 *		(i.e., strings not allocated by this module) can be
 *		referenced through descriptors.
 *
 *		Strings are allocated out of memory pools.  Three pools
 *		are maintained here, 'small', 'medium', and 'large'.
 *		When a request is made to allocate space for a string,
 *		the length specified in the request is used to select the
 *		pool, based on the SMALLSZ, MEDSZ, and LRGSZ definitions
 *		below.  Lookaside lists are kept with each pool, so
 *		string cells will get reused after being freed.  The
 *		lists are pre-allocated with ALLOCOUNT entries at
 * 		initialization time.
 *
 *		XXX The choice of sizes here is based on an educated guess
 *		of string-size frequency in a typical compilation.  Some
 *		instrumentation and analysis is called for to tune the
 *		cell sizes for each pool -- or to select a different
 *		implementation, if that would provide a better balance
 *		between heap usage/fragmentation and speed.
 *
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		21-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include "blissc/support/strings.h"

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

struct extenthdr_s {
    struct extenthdr_s *next;
};

struct strctx_s {
    struct stringpool_s pool[POOL_COUNT];
    struct extenthdr_s *extents;
};

static struct stringpool_s  pool[POOL_COUNT] = {
    { 0, SMALLSZ }, { 0, MEDSZ }, { 0, LRGSZ }
};

/*
 * Internal utility routines.
 */
static inline int is_static (strdesc_t *dsc) {
	return ((dsc->flags & STR_M_STATIC) != 0);
}

static inline int size_to_pool (size_t len) {
    int i;
    for (i = 0; i < POOL_COUNT; i++) {
        if (len <= pool[i].maxsize) {
            return i;
        }
    }
    return POOL_NONE;
}

static inline string_t *desc_to_str (strdesc_t *dsc) {
    return is_static(dsc) ? 0 : (string_t *)(dsc->ptr - sizeof(string_t));
}


/*
 * stringpool_expand
 *
 * Add more entries to a string pool's lookaside list.
 */
static int
stringpool_expand (strctx_t strctx, int idx)
{
    size_t maxsize = strctx->pool[idx].maxsize;
    struct extenthdr_s *extent;
    string_t *more;
    string_t *str;
    char *cp;
    int i;

    extent = malloc(sizeof(struct extenthdr_s) + (maxsize + sizeof(string_t))*ALLOCOUNT);
    if (extent == 0) {
        return 0;
    }
    extent->next = strctx->extents;
    strctx->extents = extent;
    more = (string_t *)(extent + 1);

    for (cp = (char *)more, i = 0; i < ALLOCOUNT;
         i++, cp += pool[idx].maxsize) {
        str = (string_t *) cp;
        cp += sizeof(string_t);
        str->poolindex = idx;
        str->next = (string_t *)(cp + maxsize);
        str->desc.flags = 0;
        str->desc.len = 0;
        str->desc.ptr = cp;
    }
    // on exit, 'str' still points to the last allocated entry
    str->next = strctx->pool[idx].freelist;
    strctx->pool[idx].freelist = (string_t *) more;
    return 1;

} /* stringpool_expand */


/*
 * strings_init
 *
 * Initialize the string pools.
 */
strctx_t
strings_init (void)
{
    int i;
    strctx_t sctx = malloc(sizeof(struct strctx_s));

    if (sctx != 0) {
        memset(sctx, 0, sizeof(struct strctx_s));
        sctx->pool[0].maxsize = SMALLSZ;
        sctx->pool[1].maxsize = MEDSZ;
        sctx->pool[2].maxsize = LRGSZ;
        for (i = 0; i < POOL_COUNT; i++) {
            if (!stringpool_expand(sctx, i)) {
                return 0;
            }
        }
    }
    return sctx;

} /* strings_init */

/*
 * strings_finish
 *
 * Release all string memory.
 */
void
strings_finish (strctx_t strctx)
{
    struct extenthdr_s *e, *enext;

    if (strctx == 0) {
        return;
    }

    for (e = strctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }

    free(strctx);

} /* strings_finish */

/*
 * string___alloc
 *
 * Internal allocation routine.  Identifies
 * the string pool and triggers its expansion,
 * if needed.
 */
static string_t *
string___alloc (strctx_t strctx, size_t len)
{
    string_t *str;
    int i = size_to_pool(len);

    if (i == POOL_NONE) {
        return 0;
    }
    if (strctx->pool[i].freelist == 0) {
        if (!stringpool_expand(strctx, i)) {
            return 0;
        }
    }
    str = strctx->pool[i].freelist;
    strctx->pool[i].freelist = str->next;
    str->next = 0;
    return str;

} /* string___alloc */

/*
 * string___free
 *
 * Return a string cell to its pool's lookaside list.
 */
static void
string___free (strctx_t strctx, string_t *str)
{
    if (str->poolindex == POOL_NONE) {
        return;
    }
    str->next = strctx->pool[str->poolindex].freelist;
    strctx->pool[str->poolindex].freelist = str;

} /* string___free */

/*
 * string_from_chrs
 *
 * Allocates a dynamic string from a C-style character pointer/
 * length pair, and returns a pointer to a descriptor for the
 * string.
 *
 */
strdesc_t *
string_from_chrs (strctx_t strctx, strdesc_t *dest, const char *cp, size_t len)
{
    string_t *str = string___alloc(strctx, len);

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

} /* string_from_chrs */

/*
 * ascic_string_from_chrs
 *
 * Allocates a dynamic string in %ASCIC form - that is,
 * prefixed with a length byte.
 */
strdesc_t *
ascic_string_from_chrs (strctx_t strctx, strdesc_t *dest, const char *cp, size_t len)
{
    string_t *str = string___alloc(strctx, len+1);
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

} /* ascic_string_from_chrs */

/*
 * string_append
 *
 * Appends one string to another.  Will allocate
 * a new string (and descriptor) if the concatenated
 * string is too long to fit into the destination's
 * current cell size, so callers must use the pointer
 * returned by this routine to access the result.
 */
strdesc_t *
string_append (strctx_t strctx, strdesc_t *trg, strdesc_t *add)
{
    string_t *str;
    string_t *newstr;

    if (trg == 0) {
        return string_copy(strctx, trg, add);
    }
    str = desc_to_str(trg);
    if (is_static(trg) ||
        (trg->len + add->len <= pool[str->poolindex].maxsize)) {
        memcpy(trg->ptr + trg->len, add->ptr, add->len);
        trg->len += add->len;
        return trg;
    }
    newstr = string___alloc(strctx, trg->len + add->len);
    if (newstr == 0) {
        return 0;
    }
    newstr->desc.len = trg->len + add->len;
    memcpy(newstr->desc.ptr, trg->ptr, trg->len);
    memcpy(newstr->desc.ptr + trg->len, add->ptr, add->len);
    string___free(strctx, str);
    return &newstr->desc;

} /* string_append */

/*
 * string_free
 *
 * Public API for freeing a string via a string
 * descriptor.  It's OK to pass a null pointer or
 * a pointer to a static string descriptor; they will
 * be silently ignored.
 */
void
string_free (strctx_t strctx, strdesc_t *dsc)
{
    if (dsc == 0 || is_static(dsc)) {
        return;
    }
    string___free(strctx, desc_to_str(dsc));

} /* string_free */

/*
 * string_alloc
 *
 * Public API for allocating a string and descriptor
 * for a string cell that will hold at least 'len'
 * characters.  If a destination descriptor is
 * provided, it will be set to point to the allocated
 * string.
 */
strdesc_t *
string_alloc (strctx_t strctx, strdesc_t *dest, size_t len)
{
    string_t *str = string___alloc(strctx, len);
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

} /* string_alloc */

/*
 * string_copy
 *
 * Makes a copy of a string.  Returns a pointer
 * to the copy's descriptor (which will be the 'dest'
 * descriptor, if 'dest' is non-null).
 */
strdesc_t *
string_copy (strctx_t strctx, strdesc_t *dest, strdesc_t *src)
{
    string_t *str;

    if (dest != 0 && !is_static(dest)) {
        str = desc_to_str(dest);
        if (strctx->pool[str->poolindex].maxsize >= src->len) {
            dest->len = src->len;
            memcpy(dest->ptr, src->ptr, src->len);
            return dest;
        }
        string___free(strctx, str);
    }
    str = string___alloc(strctx, src->len);
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

} /* string_copy */

/*
 * strings_eql
 *
 * Returns 1 if the two strings are the same
 * length and compare for equality, zero otherwise.
 */
int
strings_eql (strdesc_t *a, strdesc_t *b)
{
    if (a->len != b->len) {
        return 0;
    }
    return memcmp(a->ptr, b->ptr, a->len) == 0;

} /* strings_eql */

/*
 * string_printf
 *
 * Provides printf() formatting into a dynamically
 * allocated string.  Note that to keep things simple,
 * the sprintf() is done into a local buffer first,
 * and then copied to the dynamic string.
 */
strdesc_t *
string_printf (strctx_t strctx, strdesc_t *dst, const char *fmt, ...)
{
    va_list ap;
    char buf[256];
    int len;

    va_start(ap, fmt);
    len = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    return string_from_chrs(strctx, dst, buf, (len < 0 ? 0 : len));

} /* string_printf */

/*
 * string_numval
 *
 * Uses strtol() to parse a numeric value from a string.
 * Any radix value supported by strtol() may be specified
 * for 'base', but typical values are 8, 10, and 16.
 */
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

} /* string_numval */
