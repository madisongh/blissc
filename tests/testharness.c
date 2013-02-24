/*++
 * testharness.c
 *
 * Simple test harness for running BLISS tests.
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

int TEST_INIT(void);
void RUNTEST(int);

void
TEST_PRINTF (const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);

    vprintf(fmt, ap);

}

int
main (int argc, char *argv[])
{
    int i, numtests;

    numtests = TEST_INIT();
    if (argc < 2) {
        for (i = 1; i <= numtests; i++)
            RUNTEST(i);
        return 0;
    }

    for (i = 1; i < argc; i++) {
        int tno = atoi(argv[i]);
        if (tno >= 1 && tno <= numtests) {
            RUNTEST(tno);
        }
    }

    return 0;
}
