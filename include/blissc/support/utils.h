#ifndef utils_h__
#define utils_h__
/*
 *++
 * utils.h - Miscellaneous utility functions.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license.  See LICENSE.TXT for details.
 *--
 */
#include <stdlib.h>
#include <stdint.h>

/*
 * Compiler info - version and compiling host.
 */
struct compilerinfo_s {
    unsigned int    ver_major, ver_minor;
    const char     *host_triple;
};
typedef struct compilerinfo_s compilerinfo_t;


/*
 * Text position tracking.  A textpos is a 64-bit integer, divided
 * up into a file index number (the indices are tracked by the lexer),
 * a line number, and a column number.  Every lexeme scanned carries
 * its text position; positions get saved with declarations.
 *
 * NB: If this is ported to a 32-bit host system, some adjustments
 * may need to be made.
 */
typedef uint64_t textpos_t;

#define TEXTPOS_V_COLNUM    0
#define TEXTPOS_S_COLNUM    16
#define TEXTPOS_V_LINENO    (TEXTPOS_V_COLNUM+TEXTPOS_S_COLNUM)
#define TEXTPOS_S_LINENO    32
#define TEXTPOS_V_FILENO    (TEXTPOS_V_LINENO+TEXTPOS_S_LINENO)
#define TEXTPOS_S_FILENO    16
#define TEXTPOS_V_UNUSED    (TEXTPOS_V_FILENO+TEXTPOS_S_FILENO)
#define TEXTPOS_S_UNUSED    (sizeof(textpos_t)*8-TEXTPOS_V_UNUSED)

static inline __attribute__((unused)) textpos_t textpos_create(int fno, unsigned int lno,
                                                unsigned int cno) {
    return (textpos_t) (
                (((uint64_t) fno & ~(~0ULL<<TEXTPOS_S_FILENO)) << TEXTPOS_V_FILENO)
            | (((uint64_t) lno & ~(~0ULL<<TEXTPOS_S_LINENO)) << TEXTPOS_V_LINENO)
            | (((uint64_t) cno & ~(~0ULL<<TEXTPOS_S_COLNUM)) << TEXTPOS_V_COLNUM)
                        );
}
static inline __attribute__((unused)) unsigned int textpos_fileno(textpos_t t) {
    return (unsigned int)((t >> TEXTPOS_V_FILENO) & ~(~0ULL<<TEXTPOS_S_FILENO));
}
static inline __attribute__((unused)) unsigned int textpos_lineno(textpos_t t) {
    return (unsigned int)((t >> TEXTPOS_V_LINENO) & ~(~0ULL<<TEXTPOS_S_LINENO));
}
static inline __attribute__((unused)) unsigned int textpos_colnum(textpos_t t) {
    return (unsigned int)((t >> TEXTPOS_V_COLNUM) & ~(~0ULL<<TEXTPOS_S_COLNUM));
}

/*
 * initval_t - need the opaque type definition here XXX
 */
struct initval_s;
typedef struct initval_s initval_t;

/*
 * Width-related functions.  These handle things like bit counting and
 * translation of integers between the host's "long" format and the target's
 * specific width/sign-extension format.
 *
 * NB: This code cannot handle widths larger than the size of a 'long' on
 * the host system.
 */
long bits_needed(unsigned long);

static inline __attribute__((unused)) long getvalue(long val, unsigned int width, int signext) {
    long result;
    if (width == sizeof(long)*8) {
        return val;
    }
    result = labs(val) & ((1UL<<(width-(signext == 0 ? 0 : 1)))-1);
    return (signext && (val < 0) ? -result : result);
}
static inline __attribute__((unused)) long getmvalue(long val, unsigned long mask, int signext) {
    long result;
    result = labs(val) & mask;
    return (signext && (val < 0) ? -result : result);
}

/*
 * Tail-queue implementation.  A tail-queue consists of a header
 * that tracks the head and tail of the queue, along with a count of
 * the elements in the queue.  Queue elements only include a single
 * 'next' pointer.
 *
 * TQ_HDR_FIELDS(typename) should be included in a structure declaration for
 * the tail-queue header.  The typename should be the struct tag or typedef
 * for the enclosing structure.
 *
 * TQ_ENT_FIELDS(typename) should be included in a structure declaration for
 * the elements.  The typename should be the struct tag or typedef for the
 * enclosing structure.
 *
 * DEFINE_TQ_FUNCS(prefix, hdrtype, enttype) should be used to instantiate
 * a set of inline functions for manipulating the tail queue.  The specified
 * prefix will be prepended to the function names.  The hdrtype and enttype
 * parameters should be the type names ('struct <tag>' or typedef'ed type)
 * for the queue header and queue entry structures, respectively.
 *
 * Functions (<p> represents the prefix mentioned above):
 * <p>_init:        initializes the tail-queue header fields.
 * <p>_empty:       returns 1 if the queue is empty.
 * <p>_inshead:     insert an entry at the head of the queue.
 * <p>_instail:     insert an entry at the tail of the queue.
 * <p>_append:      append one queue to another.
 * <p>_prepend:     prepend one queue at the front of another.
 * <p>_remhead:     remove the entry at the head of the queue.
 * <p>_remtail:     remove the entry at the tail of the queue.
 * <p>_remove:      remove an arbitrary entry from the queue.
 * <p>_head:        return a pointer to the first element in the queue.
 * <p>_tail:        return a pointer to the last element in the queue.
 * <p>_length:      return the number of entries in the queue.
 *
 * Note that the 'remove' function is the most expensive, requiring
 * a linear search of the queue until the entry is found.  If you need
 * to do arbitrary removes frequently, a different queue implementation
 * would probably be in order.
 *
 */
#define TQ_HDR_FIELDS(enttype_) \
    enttype_ *tq_head; \
    enttype_ *tq_tail; \
    int      tq_count;
#define TQ_ENT_FIELDS(enttype_) \
    enttype_ *tq_next;
#define DEFINE_TQ_FUNCS(pfx_, hdrtype_, enttype_) \
static inline __attribute__((unused)) void pfx_##_init (hdrtype_ *h) { \
    h->tq_head = h->tq_tail = 0; h->tq_count = 0; } \
static inline __attribute__((unused)) int pfx_##_empty (hdrtype_ *h) { return (h->tq_count == 0); } \
static inline __attribute__((unused)) void pfx_##_inshead (hdrtype_ *h, enttype_ *e) { \
    if (h->tq_count == 0) h->tq_tail = e; \
    e->tq_next = h->tq_head; h->tq_head = e; h->tq_count += 1; } \
static inline __attribute__((unused)) void pfx_##_instail (hdrtype_ *h, enttype_ *e) { \
    if (h->tq_count == 0) pfx_##_inshead(h, e); \
    else { h->tq_tail->tq_next = e; e->tq_next = 0; h->tq_tail = e; h->tq_count += 1;}} \
static inline __attribute__((unused)) void pfx_##_append (hdrtype_ *dst, hdrtype_ *addon) { \
    if (addon->tq_count == 0) return; \
    if (dst->tq_count == 0) { \
        dst->tq_head = addon->tq_head; dst->tq_tail = addon->tq_tail; \
        dst->tq_count = addon->tq_count; } \
    else { dst->tq_tail->tq_next = addon->tq_head; dst->tq_tail = addon->tq_tail; \
        dst->tq_count += addon->tq_count; } \
    addon->tq_head = addon->tq_tail = 0; addon->tq_count = 0; } \
static inline __attribute__((unused)) void pfx_##_prepend (hdrtype_ *dst, hdrtype_ *addon) { \
    if (addon->tq_count == 0) return; \
    if (dst->tq_count == 0) { \
        dst->tq_head = addon->tq_head; dst->tq_tail = addon->tq_tail; \
        dst->tq_count = addon->tq_count;} else { \
        addon->tq_tail->tq_next = dst->tq_head; dst->tq_head = addon->tq_head; \
        dst->tq_count += addon->tq_count; } \
    addon->tq_head = addon->tq_tail = 0; addon->tq_count = 0; } \
static inline __attribute__((unused)) enttype_ * pfx_##_remhead (hdrtype_ *h) { \
    enttype_ *e = h->tq_head; if (e == 0) return e; \
    h->tq_head = e->tq_next; e->tq_next = 0; h->tq_count -= 1; return e; } \
static inline __attribute__((unused)) enttype_ * pfx_##_remove (hdrtype_ *h, enttype_ *e) { \
    enttype_ *p = h->tq_head; if (p == 0) return 0; \
    if (h->tq_count == 1) { if (p != e) return 0; \
        h->tq_head = h->tq_tail = 0; h->tq_count = 0; return e; } \
    while (p->tq_next != e) { p = p->tq_next; } \
    p->tq_next = e->tq_next; h->tq_count -= 1; \
    if (h->tq_tail == e) h->tq_tail = p; \
    return e; } \
static inline __attribute__((unused)) enttype_ * pfx_##_remtail (hdrtype_ *h) { \
     return pfx_##_remove(h, h->tq_tail); } \
static inline __attribute__((unused)) enttype_ * pfx_##_head (hdrtype_ *h) { return h->tq_head; } \
static inline __attribute__((unused)) enttype_ * pfx_##_tail (hdrtype_ *h) { return h->tq_tail; } \
static inline __attribute__((unused)) int pfx_##_length (hdrtype_ *h) { return h->tq_count; }

#endif /* utils_h__ */
