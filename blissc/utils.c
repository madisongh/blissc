//
//  utils.c
//  blissc
//
//  Utility routines
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>

/*
 * bits_needed
 *
 * Calculates the number of bits needed to hold
 * a binary integer value.
 */
long
bits_needed (unsigned long val) {
    long count = 0;
    while (val != 0) {
        count += 1;
        val = val >> 1;
    }
    return count;
} /* bits_needed */
