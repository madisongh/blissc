#include <stdio.h>
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
main (void)
{
    int i, numtests;

    numtests = TEST_INIT();
    for (i = 1; i <= numtests; i++)
        RUNTEST(i);
    return 0;
}
