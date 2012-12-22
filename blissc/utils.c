/*
 *++
 *	File:			utils.c
 *
 *	Abstract:		Miscellaneous utility functions
 *
 *  Module description:
 *		This module contains miscellaneous utility functions
 *		used by other modules.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		 20-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>

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
