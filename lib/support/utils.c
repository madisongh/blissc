/*
 *++
 * utils.c - Miscellaneous utility functions
 *
 * This module contains miscellaneous utility functions
 * used by other modules.
 *
 * Copyright © 2012-2020, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

/*
 * bits_needed
 *
 * Calculates the number of bits needed to hold
 * a binary integer value.
 */
long
bits_needed (unsigned long val)
{
    long count = 0;
    while (val != 0) {
        count += 1;
        val = val >> 1;
    }
    return count;
} /* bits_needed */
