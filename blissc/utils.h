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
    long result = labs(val) & ((1UL<<width)-(signext == 0 ? 0 : 1));
    return (signext && (val < 0) ? -result : result);
}
static inline __unused long getmvalue(long val, unsigned long mask, int signext) {
    long result = labs(val) & mask;
    return (signext && (val < 0) ? -result : result);
}

#endif
