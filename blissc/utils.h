//
//  utils.h
//  blissc
//
//  Created by Matthew Madison on 10/28/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_utils_h
#define blissc_utils_h

#include <string.h>

typedef struct {
    unsigned short flags;
#define STR_M_STATIC (1<<0)
    unsigned short len;
    char           *ptr;
} strdesc_t;

#define STRDEF(s) { STR_M_STATIC, sizeof(s)-1, s }
#define STRZDEF(s) { STR_M_STATIC, sizeof(s), s }
#define INITSTR(dsc, s, l) do { \
    (dsc).flags = STR_M_STATIC; \
    (dsc).len = (l); (dsc).ptr = (s); \
} while(0)

static inline __unused strdesc_t *
strdesc_init (strdesc_t *dsc, char *s, size_t len)
{
    dsc->flags = STR_M_STATIC;
    dsc->len   = len;
    dsc->ptr   = s;
    return dsc;
}

union data_u {
    void            *ptr;
    unsigned long    val_unsigned;
    long             val_signed;
    strdesc_t        val_string;
};
typedef union data_u data_t;

#endif
