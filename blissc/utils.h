//
//  utils.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_utils_h
#define blissc_utils_h

#include <stdlib.h>
#include <stdint.h>

/*
 * Text positions
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

static inline __unused textpos_t textpos_create(int fno, unsigned int lno,
                                                unsigned int cno) {
    return (textpos_t)(((uint64_t)fno & ~(1LLU<<TEXTPOS_S_FILENO)) << TEXTPOS_V_FILENO)
                    | (((uint64_t)lno & ~(1LLU<<TEXTPOS_S_LINENO)) << TEXTPOS_V_LINENO)
                    | (((uint64_t)cno & ~(1LLU<<TEXTPOS_S_COLNUM)) << TEXTPOS_S_COLNUM);
}
static inline __unused int textpos_fileno(textpos_t t) {
    return (int)((t >> TEXTPOS_V_FILENO) & ~(1LLU<<TEXTPOS_S_FILENO));
}
static inline __unused int textpos_lineno(textpos_t t) {
    return (int)((t >> TEXTPOS_V_LINENO) & ~(1LLU<<TEXTPOS_S_LINENO));
}
static inline __unused int textpos_colnum(textpos_t t) {
    return (int)((t >> TEXTPOS_V_COLNUM) & ~(1LLU<<TEXTPOS_S_COLNUM));
}


long bits_needed(unsigned long);

static inline __unused long getvalue(long val, unsigned int width, int signext) {
    long result = labs(val) & ((1UL<<(width-(signext == 0 ? 0 : 1)))-1);
    return (signext && (val < 0) ? -result : result);
}
static inline __unused long getmvalue(long val, unsigned long mask, int signext) {
    long result = labs(val) & mask;
    return (signext && (val < 0) ? -result : result);
}

// Singly-linked tail queue implementation, as inline functions

#define TQ_HDR_FIELDS(enttype_) \
    enttype_ *tq_head; \
    enttype_ *tq_tail; \
    int      tq_count;
#define TQ_ENT_FIELDS(enttype_) \
    enttype_ *tq_next;
#define DEFINE_TQ_FUNCS(pfx_, hdrtype_, enttype_) \
static inline __unused void pfx_##_init (hdrtype_ *h) { \
    h->tq_head = h->tq_tail = 0; h->tq_count = 0; } \
static inline __unused int pfx_##_empty (hdrtype_ *h) { return (h->tq_count == 0); } \
static inline __unused void pfx_##_inshead (hdrtype_ *h, enttype_ *e) { \
    if (h->tq_count == 0) h->tq_tail = e; \
    e->tq_next = h->tq_head; h->tq_head = e; h->tq_count += 1; } \
static inline __unused void pfx_##_instail (hdrtype_ *h, enttype_ *e) { \
    if (h->tq_count == 0) pfx_##_inshead(h, e); \
    else { h->tq_tail->tq_next = e; e->tq_next = 0; h->tq_tail = e; h->tq_count += 1;}} \
static inline __unused void pfx_##_append (hdrtype_ *dst, hdrtype_ *addon) { \
    if (addon->tq_count == 0) return; \
    if (dst->tq_count == 0) { \
        dst->tq_head = addon->tq_head; dst->tq_tail = addon->tq_tail; \
        dst->tq_count = addon->tq_count; } \
    else { dst->tq_tail->tq_next = addon->tq_head; dst->tq_tail = addon->tq_tail; \
        dst->tq_count += addon->tq_count; } \
    addon->tq_head = addon->tq_tail = 0; addon->tq_count = 0; } \
static inline __unused void pfx_##_prepend (hdrtype_ *dst, hdrtype_ *addon) { \
    if (addon->tq_count == 0) return; \
    if (dst->tq_count == 0) { \
        dst->tq_head = addon->tq_head; dst->tq_tail = addon->tq_tail; dst->tq_count = addon->tq_count;} \
    else { addon->tq_tail->tq_next = dst->tq_head; dst->tq_head = addon->tq_head; dst->tq_count += addon->tq_count; } \
    addon->tq_head = addon->tq_tail = 0; addon->tq_count = 0; } \
static inline __unused enttype_ * pfx_##_remhead (hdrtype_ *h) { \
    enttype_ *e = h->tq_head; if (e == 0) return e; \
    h->tq_head = e->tq_next; e->tq_next = 0; h->tq_count -= 1; return e; } \
static inline __unused enttype_ * pfx_##_remove (hdrtype_ *h, enttype_ *e) { \
    enttype_ *p = h->tq_head; if (p == 0) return 0; \
    if (h->tq_count == 1) { if (p != e) return 0; \
        h->tq_head = h->tq_tail = 0; h->tq_count = 0; return e; } \
    while (p->tq_next != e) { p = p->tq_next; } \
    p->tq_next = e->tq_next; h->tq_count -= 1; \
    if (h->tq_tail == e) h->tq_tail = p; \
    return e; } \
static inline __unused enttype_ * pfx_##_remtail (hdrtype_ *h) { \
     return pfx_##_remove(h, h->tq_tail); } \
static inline __unused enttype_ * pfx_##_head (hdrtype_ *h) { return h->tq_head; } \
static inline __unused enttype_ * pfx_##_tail (hdrtype_ *h) { return h->tq_tail; } \
static inline __unused int pfx_##_length (hdrtype_ *h) { return h->tq_count; }

#endif
