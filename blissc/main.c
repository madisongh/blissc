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

void declarations_init(scopectx_t scope);
int parse_declaration(parse_ctx_t pctx, lextype_t lt);

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
    strdesc_t *tok;
    scantype_t toktype;
    unsigned int sflags = SCAN_M_SIGNOK;
    unsigned int lno, cno;
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
        toktype = scan_getnext(ctx, sflags, &tok, &lno, &cno);
        if (scan_ok(toktype)) {
            printf("<%u:%u> ", lno, cno);
            switch (toktype) {
                case SCANTYPE_DECLITERAL:
                    printf("Decimal literal: %-*.*s\n", tok->len,tok->len,tok->ptr);
                    sflags &= ~SCAN_M_SIGNOK;
                    break;
                case SCANTYPE_IDENTIFIER:
                    printf("Identifier:      %-*.*s\n", tok->len,tok->len,tok->ptr);
                    sflags &= ~SCAN_M_SIGNOK;
                    break;
                case SCANTYPE_OPERATOR:
                    printf("Operator:        %-*.*s\n", tok->len,tok->len,tok->ptr);
                    sflags |= SCAN_M_SIGNOK;
                    break;
                case SCANTYPE_PUNCTUATION:
                    printf("Punctuation:     %-*.*s\n", tok->len,tok->len,tok->ptr);
                    if (strchr("]>)", *tok->ptr) != 0) {
                        sflags &= ~SCAN_M_SIGNOK;
                    } else {
                        sflags |= SCAN_M_SIGNOK;
                    }
                    break;
                case SCANTYPE_QUOTEDSTRING:
                    printf("Quoted string:   %-*.*s\n", tok->len,tok->len,tok->ptr);
                    sflags |= SCAN_M_SIGNOK;
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

static int linewidth = 0;
static char *delim = "";


void PRINTCR (void)
{
    printf("\n");
    linewidth = 0;
    delim = "";
}

void PRINTLEX(lexeme_t *lex)
{
    if (lex->text.len == 0) {
        const char *cp = lextype_name(lexeme_boundtype(lex));//+sizeof("LEXTYPE");
        printf("%s%s", delim, cp);
        linewidth += strlen(cp);
    } else {
        printf("%s%-*.*s", delim, lex->text.len, lex->text.len, lex->text.ptr);
        linewidth += lex->text.len;
    }
    if (linewidth > 60) {
        delim = "\n";
        linewidth = 0;
    } else {
        delim = " ";
    }
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
    lextype_t lt;
    int linewidth;
    char *delim;

    mainscope = scope_begin(0);
    pctx = parser_init(mainscope, 0);
    declarations_init(mainscope);
    if (!parser_fopen(pctx, argv[0], strlen(argv[0]))) {
        fprintf(stderr, "parser_fopen failed for %s\n", argv[0]);
        return 998;
    }
    linewidth = 0;
    delim = "";
    for (lt = parser_next(pctx, QL_NORMAL, &lex); lt != LEXTYPE_END && lt != LEXTYPE_NONE;
         lt = parser_next(pctx, QL_NORMAL, &lex)) {
        PRINTLEX(lex);
        if (lt >= LEXTYPE_DCL_MIN || lt <= LEXTYPE_DCL_MAX) {
            parse_declaration(pctx, lt);
        }
        lexeme_free(lex);
    }
    if (lt == LEXTYPE_NONE) {
        fprintf(stderr, "parser_next returned error lexeme\n");
        return 997;
    } else {
        printf("<<end of input>>\n");
    }
    parser_finish(pctx);
    scope_end(mainscope);
    return 0;
}
