/*
 *++
 *	File:			listings.c
 *
 *	Abstract:		Output handling for listings
 *
 *  Module description:
 *		This module implements compiler listing output,
 *      including tracing, assembly listings, etc.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		25-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */

#include <stdio.h>
#include "listings.h"
#include "switches.h"
#include "fileio.h"

struct liststate_s {
    struct liststate_s *next;
    scopectx_t          this_scope;
    unsigned int        listopts[LISTOPT_COUNT];
};
typedef struct liststate_s liststate_t;

#define NAME_POS    0
#define NAME_LEN    15
#define TITLE_POS   17
#define TITLE_LEN   47
#define COMPID_POS  65
#define COMPID_LEN  57 // 67 - length of " Page nnnn"
#define PAGENO_POS  122
#define PAGENO_LEN  10

#define IDENT_POS   0
#define IDENT_LEN   15
#define SBTTL_POS   17
#define SBTTL_LEN   47
#define FNAME_POS   65
#define FNAME_LEN   67

#define LINESPERPAGE 60

struct lstgctx_s {
    fioctx_t    fio;
    filectx_t   outf;
    char        main_input[FNAME_LEN];
    liststate_t *cur_state;
    liststate_t  main_state;
    unsigned int blockdepth;
    unsigned int pagenum;
    unsigned int nlines;
    int          require_depth;
    int          haveabuf;
    size_t       linelen;
    char         header1[132];
    char         header2[132];
    char         linebuf[132];
};

static strdesc_t loptswitch[LISTOPT_COUNT] = {
    STRDEF("SOURCE"), STRDEF("REQUIRE"), STRDEF("EXPAND"), STRDEF("TRACE"),
    STRDEF("LIBRARY"), STRDEF("OBJECT"), STRDEF("ASSEMBLY"), STRDEF("SYMBOLIC"),
    STRDEF("BINARY"), STRDEF("COMMENTARY")
};

static lextype_t lopt_lextypes[] = {
    LEXTYPE_NAME_TOGGLE_OFF, LEXTYPE_NAME_TOGGLE_ON,
    LEXTYPE_DCL_REQUIRE, LEXTYPE_DCL_LIBRARY
};

static int
listopt_handler (parse_ctx_t pctx, void *vctx, int togidx, lextype_t dtype, name_t *togname)
{
    lstgctx_t ctx = vctx;
    int set_on = (name_type(togname) == LEXTYPE_NAME_TOGGLE_ON);

    ctx->cur_state->listopts[togidx] = set_on;
    // If we aren't putting source in the listing, tell
    // the logging module to print the source line
    if (togidx == LISTOPT_SRC) {
        log_logsrctolst_set(parser_logctx(pctx), !set_on);
    }
    return 1;

} /* listopt_handler */

static int
list_switch_handler (parse_ctx_t pctx, void *vctx, lextype_t dtype, lexeme_t *swlex)
{
    lstgctx_t ctx = vctx;
    lexeme_t *lex;
    liststate_t *newstate;
    int i;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, "(");
        return 0;
    }

    if (parser_scope_get(pctx) != ctx->cur_state->this_scope) {
        newstate = malloc(sizeof(liststate_t));
        if (newstate == 0) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__INTCMPERR,
                       "list_switch_handler");
            return 0;
        }
        memcpy(newstate, ctx->cur_state, sizeof(liststate_t));
        newstate->next = ctx->cur_state;
        newstate->this_scope = parser_scope_get(pctx);
        ctx->cur_state = newstate;
    } else {
        newstate = 0;
    }

    while (1) {
        i = parser_expect_oneof(pctx, QL_NORMAL, lopt_lextypes,
                                sizeof(lopt_lextypes)/sizeof(lopt_lextypes[0]), &lex, 1);
        if (i < 0) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__LSTOPTEXP);
            break;
        }
        if (lexeme_type(lex) == LEXTYPE_DCL_REQUIRE) {
            ctx->cur_state->listopts[LISTOPT_REQ] = 1;
        } else if (lexeme_type(lex) == LEXTYPE_DCL_LIBRARY) {
            ctx->cur_state->listopts[LISTOPT_LIB] = 1;
        } else {
            value_toggle_dispatch(pctx, ctx, dtype, lexeme_ctx_get(lex));
        }
        lexeme_free(parser_lexmemctx(pctx), lex);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            return 1;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ",");
            break;
        }
    } /* while */

    // We break out of the loop only on error

    if (newstate != 0) {
        ctx->cur_state = newstate->next;
        free(newstate);
    }
    parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
    return 0;

} /* list_switch_handler */

/*
 * listing_newblock
 *
 * Called at the beginning of a block, so we increment the block depth.
 */
void
listing_newblock (lstgctx_t ctx)
{
    ctx->blockdepth += 1;
}
/*
 * listing_endblock
 *
 * Called at the end of a block, just before the scope is destroyed,
 * so we can see if we have to pop listing options off our stack, and
 * to decrement the block depth.
 */
void
listing_endblock (lstgctx_t ctx, scopectx_t this_scope)
{
    liststate_t *l = ctx->cur_state;

    ctx->blockdepth -= 1;

    if (l->this_scope != this_scope) {
        return;
    }
    ctx->cur_state = l->next;
    free(l);

} /* listing_endblock */

/*
 * listing_require_begin
 *
 * Called when a REQUIRE file is opened, so we can record its name.
 * We also advance to the end of the page, so the new file begins
 * on a new page, but only if LISTOPT_REQ is set.
 */
void
listing_require_begin (lstgctx_t ctx, char *fname)
{
    if (ctx->outf == 0) return;

    ctx->require_depth += 1;
    if (ctx->cur_state->listopts[LISTOPT_REQ]) {
        size_t len = strlen(fname);
        if (len > FNAME_LEN) len = FNAME_LEN;
        memcpy(ctx->header2+FNAME_POS, fname, len);
        memset(ctx->header2+(FNAME_POS+len), ' ', FNAME_LEN-len);
        ctx->nlines = LINESPERPAGE;
    }

} /* listing_require_begin */

/*
 * listing_file_close
 *
 * Called when a file is closed, through a hook in the scanner.
 * If require_depth > 0, we've just closed a REQUIRE file.
 * Otherwise, we're closing the main file, and should finish up
 * the source listing output, if there is any pending.
 */
void
listing_file_close (void *vctx)
{
    lstgctx_t ctx = vctx;

    if (ctx->outf == 0) return;
    if (ctx->require_depth > 0) {
        ctx->require_depth -= 1;
        if (ctx->require_depth == 0) {
            if (ctx->cur_state->listopts[LISTOPT_REQ]) {
                memcpy(ctx->header2+FNAME_POS, ctx->main_input, FNAME_LEN);
                ctx->nlines = LINESPERPAGE;
            }
        }
    }

} /* listing_require_end */

/*
 * listing_open
 *
 * Called when we open the main input file and want to generate
 * a listing file.
 */
int
listing_open (lstgctx_t ctx, const char *mainfile)
{
    const char *cp;
    size_t llen, len;
    llen = len = strlen(mainfile);
    for (cp = mainfile+(len-1); cp >= mainfile && *cp != '.'; cp--);
    if (cp >= mainfile) llen = (cp - mainfile);
    ctx->outf = file_open_output(ctx->fio, mainfile, llen, ".lis");
    if (ctx->outf == 0) {
        return 0;
    }
    if (len > FNAME_LEN) len = FNAME_LEN;
    memcpy(ctx->main_input, mainfile, len);
    memset(ctx->main_input+len, ' ', FNAME_LEN-len);
    memcpy(ctx->header2+FNAME_POS, ctx->main_input, FNAME_LEN);
    ctx->nlines = LINESPERPAGE;

    return 1;

} /* listing_open */


/*
 * listing_printline
 *
 * Handles the page formatting.  All output should go through
 * this routine.  len is expected to be <= page width.
 */
static int
printline (lstgctx_t ctx, const char *buf, size_t len)
{
    if (ctx->outf == 0) return 1;

    if (ctx->haveabuf) {
        if (ctx->nlines >= LINESPERPAGE) {
            char pagenumstr[PAGENO_LEN-4];
            if (ctx->pagenum > 0) file_writeline(ctx->outf, "\f", 1);
            ctx->pagenum += 1;
            if (snprintf(pagenumstr, sizeof(pagenumstr), "%*u", PAGENO_LEN-5, ctx->pagenum) > 0) {
                memcpy(ctx->header1+PAGENO_POS+5, pagenumstr, PAGENO_LEN-5);
            }
            file_writeline(ctx->outf, ctx->header1, sizeof(ctx->header1));
            file_writeline(ctx->outf, ctx->header2, sizeof(ctx->header2));
            file_writeline(ctx->outf, "", 0);
            ctx->nlines = 3;
        }
        file_writeline(ctx->outf, ctx->linebuf, ctx->linelen);
        ctx->nlines += 1;
    }
    if (buf != 0) {
        ctx->linelen = (len > sizeof(ctx->linebuf) ? sizeof(ctx->linebuf) : len);
        memcpy(ctx->linebuf, buf, ctx->linelen);
        ctx->haveabuf = 1;
    }

    return 1;

} /* printline */

/*
 * listing_printline
 *
 * General print routine or use by hooks in other modules.
 */
void
listing_printline (void *ctx, const char *buf, size_t len, int align_with_source)
{
    char outbuf[132];
    if (align_with_source) {
        if (len > 132-16) len = 132-16;
        memset(outbuf, ' ', 16);
        memcpy(outbuf+16, buf, len);
        printline(ctx, outbuf, len+16);
    } else {
        if (len > 132) len = 132;
        printline(ctx, buf, len);
    }

} /* listing_printline */

/*
 * listing_printsrc
 *
 * Called for printing a source line (via hook in the scanner)
 */
int
listing_printsrc (void *vctx, char *buf, size_t len, unsigned int lineno, char lexcode)
{
    lstgctx_t ctx = vctx;
    char outbuf[132];
    int n;

    if (ctx->outf == 0 || !ctx->cur_state->listopts[LISTOPT_SRC]) return 1;
    if (ctx->require_depth > 0 && !ctx->cur_state->listopts[LISTOPT_REQ]) return 1;
    if (len > 132-16) len = 132-16;
    n = snprintf(outbuf, sizeof(outbuf), "; %c%c %5u %2u   %-*.*s",
                 lexcode, (ctx->require_depth == 0 ? ' ' : 'R'), lineno, ctx->blockdepth,
                 (int)len, (int)len, buf);
    if (n < 0) {
        return 0;
    }
    printline(ctx, outbuf, n);
    return 1;

} /* listing_printsrc */

/*
 * listings_init
 *
 * Module initialization.
 */
lstgctx_t
listings_init (scopectx_t kwdscope, logctx_t logctx)
{
    lstgctx_t ctx = malloc(sizeof(struct lstgctx_s));
    name_t *onp, *offp;
    int i;

    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct lstgctx_s));
        ctx->cur_state = &ctx->main_state;
        ctx->fio = fileio_init(logctx);
        memset(ctx->header1, ' ', sizeof(ctx->header1));
        memcpy(ctx->header1+PAGENO_POS, "Page", 4);
        memset(ctx->header2, ' ', sizeof(ctx->header2));
        ctx->main_state.listopts[LISTOPT_SRC] = 1;
    }

    for (i = 0; i < sizeof(loptswitch)/sizeof(loptswitch[0]); i++) {
        if (i == 1 || i == 4) { // REQUIRE and LIBRARY
            value_toggle_declare(kwdscope, &loptswitch[i], listopt_handler, ctx, i, 0, &offp);
        } else {
            value_toggle_declare(kwdscope, &loptswitch[i], listopt_handler, ctx, i,
                                 &onp, &offp);
        }
    }
    switch_special_declare(kwdscope, LEXTYPE_SWITCH_LIST, list_switch_handler, ctx);

    return ctx;

} /* listings_init */

/*
 * listings_finish
 *
 * Module cleanup.
 */
void
listings_finish (lstgctx_t ctx)
{
    liststate_t *l, *lnext;

    if (ctx == 0) {
        return;
    }

    if (ctx->haveabuf) {
        printline(ctx, 0, 0);
    }
    
    for (l = ctx->cur_state; l != 0 && l != &ctx->main_state; l = lnext) {
        lnext = l->next;
        free(l);
    }

    if (ctx->outf != 0) {
        file_close(ctx->outf);
    }

    free(ctx);

} /* listings_finish */

/*
 * Setters
 */
void listing_mainscope_set (lstgctx_t ctx, scopectx_t mainscope) {
    ctx->main_state.this_scope = mainscope; }
void listing_title_set (lstgctx_t ctx, strdesc_t *str) {
    int len = (str->len > TITLE_LEN ? TITLE_LEN : str->len);
    memcpy(ctx->header1+TITLE_POS, str->ptr, len);
    memset(ctx->header1+(TITLE_POS+len), ' ', TITLE_LEN-len); }
void listing_subtitle_set (lstgctx_t ctx, strdesc_t *str) {
    int len = (str->len > SBTTL_LEN ? SBTTL_LEN : str->len);
    memcpy(ctx->header2+SBTTL_POS, str->ptr, len);
    memset(ctx->header2+(SBTTL_POS+len), ' ', SBTTL_LEN-len); }
void listing_compilerid_set (lstgctx_t ctx, strdesc_t *str) {
    int len = (str->len > COMPID_LEN ? COMPID_LEN : str->len);
    memcpy(ctx->header1+COMPID_POS, str->ptr, len);
    memset(ctx->header1+(COMPID_POS+len), ' ', COMPID_LEN-len); }
void listing_ident_set (lstgctx_t ctx, strdesc_t *str) {
    int len = (str->len > IDENT_LEN ? IDENT_LEN : str->len);
    memcpy(ctx->header2+IDENT_POS, str->ptr, len);
    memset(ctx->header2+(IDENT_POS+len), ' ', IDENT_LEN-len); }
void listing_name_set (lstgctx_t ctx, strdesc_t *str) {
    int len = (str->len > NAME_LEN ? NAME_LEN : str->len);
    memcpy(ctx->header1+NAME_POS, str->ptr, len);
    memset(ctx->header1+(NAME_POS+len), ' ', NAME_LEN-len); }
