//
//  main.c
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <string.h>
#include "scanner.h"

int main(int argc, const char * argv[])
{

    scanctx_t ctx;
    char itembuf[256];
    size_t len;
    scantype_t toktype;
    int keepgoing = 1;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 997;
    }

    ctx = scan_init();
    if (ctx == 0) {
        return 999;
    }
    if (!scan_fopen(ctx, argv[1], strlen(argv[1]))) {
        fprintf(stderr, "scan_fopen failed for %s\n", argv[1]);
        return 998;
    }
    while (keepgoing) {
        toktype = scan_getnext(ctx, itembuf, sizeof(itembuf)-1, &len, 0);
        itembuf[len] = '\0';
        if (scan_ok(toktype)) {
            switch (toktype) {
                case SCANTYPE_DECLITERAL:
                    printf("Decimal literal: %s\n", itembuf);
                    break;
                case SCANTYPE_IDENTIFIER:
                    printf("Identifier:      %s\n", itembuf);
                    break;
                case SCANTYPE_OPERATOR:
                    printf("Operator:        %s\n", itembuf);
                    break;
                case SCANTYPE_PUNCTUATION:
                    printf("Punctuation:     %s\n", itembuf);
                    break;
                case SCANTYPE_QUOTEDSTRING:
                    printf("Quoted string:   %s\n", itembuf);
                    break;
                case SCANTYPE_END:
                    printf("<< end of input >>\n");
                    keepgoing = 0;
                    break;
                default:
                    printf("-- unhandled token type %u --\n",
                           (unsigned int)toktype);
                    keepgoing = 0;
                    break;
            }
        } else {
            printf("***ERROR: %u\n", (unsigned int)toktype);
            keepgoing = 0;
        }
    }
    scan_finish(ctx);
}

