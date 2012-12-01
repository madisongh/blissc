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
#include "storage.h"
#include "machinedef.h"
#include "expression.h"
#include "declarations.h"

int test_scanner(int argc, const char *argv[]);
int test_parser(int argc, const char *argv[]);
int test_expr(int argc, const char *argv[]);

int main(int argc, const char * argv[])
{
    const char *which;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s scanner <filename>\n", argv[0]);
        fprintf(stderr, "Usage: %s parser  <filename>\n", argv[0]);
        fprintf(stderr, "Usage: %s expr    <filename>\n", argv[0]);
        return 997;
    }
    which = argv[1];
    argc -= 2;
    argv += 2;

    if (strcmp(which, "scanner") == 0) {
        return test_scanner(argc, argv);
    } else if (strcmp(which, "parser") == 0) {
        return test_parser(argc, argv);
    } else if (strcmp(which, "expr") == 0) {
        return test_expr(argc, argv);
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
    lextype_t lt = lexeme_type(lex);
    strdesc_t *text = lexeme_text(lex);
    const char *typename = lextype_name(lt);

    if (lt == LEXTYPE_UNBOUND) {
        const char *btname = lextype_name(lexeme_boundtype(lex));
        linewidth += printf("%sU[%s]<%-*.*s>", delim, btname,
                            text->len, text->len, text->ptr);
    } else if (lt == LEXTYPE_SEGMENT) {
        text = seg_dumpinfo(lexeme_ctx_get(lex));
        linewidth += printf("%sSEG(%-*.*s)", delim,
                            text->len, text->len, text->ptr);
        string_free(text);
    } else if (lt == LEXTYPE_EXPRESSION) {
        text = expr_dumpinfo(lexeme_ctx_get(lex));
        linewidth += printf("%sEXPR(%-*.*s)", delim,
                            text->len, text->len, text->ptr);
        string_free(text);
    } else {
        linewidth += printf("%s%s<%-*.*s>", delim, typename,
                            text->len, text->len, text->ptr);
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
    stgctx_t stg;
    lexeme_t *lex;
    lextype_t lt;
//    int linewidth;
//    char *delim;
    machinedef_t machdef = { .bpunit=8, .bpval=32, .bpaddr=32, .signext_supported=1 };

    mainscope = scope_begin(0);
    stg = storage_init(&machdef);
    pctx = parser_init(mainscope, stg, &machdef);
    if (!parser_fopen(pctx, argv[0], strlen(argv[0]))) {
        fprintf(stderr, "parser_fopen failed for %s\n", argv[0]);
        return 998;
    }
    declarations_init(pctx, mainscope, stg, &machdef);
//    linewidth = 0;
//    delim = "";
    for (lt = parser_next(pctx, QL_NORMAL, &lex); lt != LEXTYPE_END && lt != LEXTYPE_NONE;
         lt = parser_next(pctx, QL_NORMAL, &lex)) {
        PRINTLEX(lex);
        if (lt >= LEXTYPE_DCL_MIN && lt <= LEXTYPE_DCL_MAX) {
            parser_insert(pctx, lex);
            parse_declaration(pctx);
        } else {
            lexeme_free(lex);
        }
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

/*
 * test_expr
 *
 * Test expression parsing.
 */
int
test_expr (int argc, const char *argv[])
{
    parse_ctx_t pctx;
    scopectx_t mainscope;
    stgctx_t stg;
//    lexeme_t *lex;
//   lextype_t lt;
//    int linewidth;
//    char *delim;
    machinedef_t machdef = { .bpunit=8, .bpval=32, .bpaddr=32,
        .signext_supported=1, .max_align=2, .reg_count = 16 };

    mainscope = scope_begin(0);
    stg = storage_init(&machdef);
    expr_init(mainscope);
    pctx = parser_init(mainscope, stg, &machdef);
    if (!parser_fopen(pctx, argv[0], strlen(argv[0]))) {
        fprintf(stderr, "parser_fopen failed for %s\n", argv[0]);
        return 998;
    }
    declarations_init(pctx, mainscope, stg, &machdef);
//    linewidth = 0;
//    delim = "";
    if (!declare_module(pctx)) {
//    for (lt = expr_parse_next(pctx, &lex, 0); lt != LEXTYPE_END && lt != LEXTYPE_NONE;
//         lt = expr_parse_next(pctx, &lex, 0)) {
//        PRINTLEX(lex);
//        parser_insert(pctx, lex);
//       if (!parse_Expression(pctx)) {
//            lt = parser_next(pctx, QL_NORMAL, &lex);
//            fprintf(stderr, "*** parse_Expression failed, next lex is %s ***\n",
//                    lextype_name(lt));
//            lexeme_free(lex);
//            continue;
//        }
//        lt = parser_next(pctx, QL_NORMAL, &lex);
//        lexeme_free(lex);
//        parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1);
//    }
//    if (lt == LEXTYPE_NONE) {
        fprintf(stderr, "declare_module failed\n");
        return 997;
    } else {
        printf("<<end of module>>\n");
    }
    parser_finish(pctx);
    scope_end(mainscope);
    return 0;
}
