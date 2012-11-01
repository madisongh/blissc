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
#include "parser.h"
#include "lexer.h"
#include "nametable.h"
#include "lexeme.h"

int test_scanner(int argc, const char *argv[]);
int test_parser(int argc, const char *argv[]);

int main(int argc, const char * argv[])
{
    const char *which;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s scanner <filename>\n", argv[0]);
        fprintf(stderr, "Usage: %s parser  <filename>\n", argv[0]);
        return 997;
    }
    which = argv[1];
    argc -= 2;
    argv += 2;

    if (strcmp(which, "scanner") == 0) {
        return test_scanner(argc, argv);
    } else if (strcmp(which, "parser") == 0) {
        return test_parser(argc, argv);
    }

    fprintf(stderr, "*** unrecognized test: %s", which);
    return 1001;

} /* main */

/*
 * test_scanner
 * 
 * Run a file through the scanner.
 */
int
test_scanner (int argc, const char *argv[])
{
    scanctx_t ctx;
    char itembuf[256];
    size_t len;
    scantype_t toktype;
    int keepgoing = 1;
    ctx = scan_init();

    if (ctx == 0) {
        return 999;
    }
    if (!scan_fopen(ctx, argv[0], strlen(argv[0]))) {
        fprintf(stderr, "scan_fopen failed for %s\n", argv[0]);
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
    return 0;
}

/*
 * test_parser
 *
 * Test the lexical processor.
 */
int
test_parser (int argc, const char *argv[])
{
    parse_ctx_t pctx;
    scopectx_t mainscope;
    lexeme_t *lex;
    name_t *np;

    mainscope = scope_begin(0);
    pctx = parser_init(mainscope, 0);
    if (!parser_fopen(pctx, argv[0], strlen(argv[0]))) {
        fprintf(stderr, "parser_fopen failed for %s\n", argv[0]);
        return 998;
    }
    for (lex = parser_next(pctx); lex != 0 && lex->type != LEXTYPE_END; lex = parser_next(pctx)) {
        printf("%s", lextype_name(lex->type));
        switch (lex->type) {
            case LEXTYPE_NUMERIC:
                printf(": %ld\n", lex->data.val_signed);
                break;
            case LEXTYPE_STRING:
                printf(": '%-*.*s'\n", lex->data.val_string.len, lex->data.val_string.len,
                       lex->data.val_string.ptr);
                break;
            case LEXTYPE_CSTRING:
                printf(": '%-*.*s'\n", *lex->data.val_string.ptr, *lex->data.val_string.ptr,
                       lex->data.val_string.ptr+1);
                break;
            case LEXTYPE_IDENT:
                np = lex->data.ptr;
                printf(": %s\n", np->name);
                break;
            default:
                printf("\n");
                break;
        }
        lexeme_free(lex);
    }
    if (lex == 0) {
        fprintf(stderr, "parser_next returned NULL\n");
        return 997;
    }
    parser_finish(pctx);
    scope_end(mainscope);
    return 0;
}
