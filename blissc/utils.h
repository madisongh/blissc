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

union data_u {
    void            *ptr;
    unsigned long    val_unsigned;
    long             val_signed;
    char             val_chr[sizeof(long)];
    struct {
        size_t       len;
        char        *ptr;
    } val_string;
};
typedef union data_u data_t;

#endif
