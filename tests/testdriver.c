//
//  main.c
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <setjmp.h>
#include "blissc/driver.h"

int main(int argc, const char * argv[])
{
    jmp_buf retenv;
    blissc_driverctx_t cctx = 0;
    int status = 0;

    if (setjmp(retenv)) {
        status = 1;
        goto finish;
    }
    cctx = blissc_init(retenv);
    if (!blissc_target_set(cctx, 0)) {
        fprintf(stderr, "error setting target\n");
        status = 999;
        goto finish;
    }
    if (!blissc_compile(cctx, argv[1], -1)) {
        fprintf(stderr, "bliss_compile reported error\n");
        status = 998;
        goto finish;
    }
    fprintf(stderr, "<<end of module>>\n");
finish:
    blissc_finish(cctx);
    return status;
}
