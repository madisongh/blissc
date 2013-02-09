/*
 *++
 * switches.c - Management of compiler switches
 *
 * This module implements the framework for handling
 * compiler switches and the parse routine for parsing
 * the contents of a SWITCHES declaration and the switches
 * that are specified on a MODULE declaration.
 *
 * There are two basic types of switches: toggle (called 'on-off' in
 * the LRM), and special.  Toggle switches have two entries in the
 * name table: the base name of the toggle, and the "NO" form;
 * the lextype is used to differentiate the two.  As an added bonus,
 * this module supports a more generic "value" toggle that has
 * the same mechanics, for use by other modules (cf. the listings
 * module).
 *
 * Special switches can take values in a variety of formats, and
 * so require a handler to parse the switch value and take the
 * appropriate action.  Each special switch is a separate keyword
 * with its own lextype.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "blissc/switches.h"
#include "blissc/parser.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"

struct switch_special_s {
    switch_parse_handler_fn handler;
    void                    *ctx;
};

struct toggle_s {
    toggle_handler_fn handler;
    void              *ctx;
    int               togindex;
};

static namedef_t special_switches[LEXTYPE_SWITCH_MAX-LEXTYPE_SWITCH_MIN+1] = {
    NAMEDEF("IDENT", LEXTYPE_SWITCH_IDENT, 0),
    NAMEDEF("LANGUAGE", LEXTYPE_SWITCH_LANGUAGE, 0),
    NAMEDEF("LIST", LEXTYPE_SWITCH_LIST, 0),
    NAMEDEF("MAIN", LEXTYPE_SWITCH_MAIN, 0),
    NAMEDEF("OPTLEVEL", LEXTYPE_SWITCH_OPTLEVEL, 0),
    NAMEDEF("VERSION", LEXTYPE_SWITCH_VERSION, 0),
    NAMEDEF("ADDRESSING_MODE", LEXTYPE_SWITCH_ADDRMODE, 0),
    NAMEDEF("ENVIRONMENT", LEXTYPE_SWITCH_ENV, 0),
    NAMEDEF("ENTRY", LEXTYPE_SWITCH_ENTRY, 0),
    NAMEDEF("OTS", LEXTYPE_SWITCH_OTS, 0),
    NAMEDEF("OTS_LINKAGE", LEXTYPE_SWITCH_OTSLNKG, 0)
};

static strdesc_t linkage_kwd = STRDEF("LINKAGE");
static strdesc_t structure_kwd = STRDEF("STRUCTURE");

static lextype_t switch_types[] = {
    LEXTYPE_NAME_SW_TOGGLE_ON,
    LEXTYPE_NAME_SW_TOGGLE_OFF,
    LEXTYPE_DCL_STRUCTURE,
    LEXTYPE_DCL_LINKAGE,
    LEXTYPE_SWITCH_IDENT,
    LEXTYPE_SWITCH_LANGUAGE,
    LEXTYPE_SWITCH_LIST,
    LEXTYPE_SWITCH_MAIN,
    LEXTYPE_SWITCH_OPTLEVEL,
    LEXTYPE_SWITCH_VERSION,
    LEXTYPE_SWITCH_ADDRMODE,
    LEXTYPE_SWITCH_ENV,
    LEXTYPE_SWITCH_ENTRY,
    LEXTYPE_SWITCH_OTS,
    LEXTYPE_SWITCH_OTSLNKG
};

/*
 * switch_special_declare
 *
 * Declares one of the "special" switch names and provides
 * a handler for it.
 */
int
switch_special_declare (scopectx_t scope, lextype_t swtype,
                        switch_parse_handler_fn handler, void *ctx)
{
    name_t *np;
    struct switch_special_s *data;
    namedef_t *ndef;

    if (swtype != LEXTYPE_DCL_LINKAGE && swtype != LEXTYPE_DCL_STRUCTURE &&
        (swtype < LEXTYPE_SWITCH_MIN || swtype > LEXTYPE_SWITCH_MAX)) {
        return 0;
    }
    if (swtype == LEXTYPE_DCL_LINKAGE) {
        np = name_search(scope, linkage_kwd.ptr, linkage_kwd.len, 0);
    } else if (swtype == LEXTYPE_DCL_STRUCTURE) {
        np = name_search(scope, structure_kwd.ptr, structure_kwd.len, 0);
    } else {
        ndef = &special_switches[swtype-LEXTYPE_SWITCH_MIN];
        np = name_declare(scope, ndef, 0, 0, 0, 0);
    }

    if (np == 0) {
        return 0;
    }

    data = name_extraspace(np);
    data->handler = handler;
    data->ctx = ctx;
    return 1;

} /* switch_special_declare */

/*
 * toggle_declare
 *
 * Declares ON and OFF toggle names for switches or values.
 * Some toggle names may conflict with other keywords; the caller
 * is expected to know these and to pass null pointers in for
 * the ON or OFF name_t pointers, as needed.
 */
static int
toggle_declare (scopectx_t scope, int is_switch, strdesc_t *name,
                toggle_handler_fn handler, void *ctx, int togidx,
                name_t **onpp, name_t **offpp)
{
    name_t *onp, *offp;
    struct toggle_s *datap;
    char namebuf[NAME_SIZE];
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = (is_switch ? LEXTYPE_NAME_SW_TOGGLE_OFF : LEXTYPE_NAME_TOGGLE_OFF);
    ndef.name = namebuf;
    ndef.namelen = (name->len > NAME_SIZE-3) ? NAME_SIZE-1 : name->len+2;
    namebuf[0] = 'N'; namebuf[1] = 'O';
    memcpy(namebuf+2, name->ptr, ndef.namelen-2);
    if (offpp != 0) {
        offp = name_declare(scope, &ndef, 0, 0, 0, &datap);
        if (offp == 0) {
            return 0;
        }
        datap->handler = handler;
        datap->ctx = ctx;
        datap->togindex = togidx;
        *offpp = offp;
    } else offp = 0;

    ndef.lt = (is_switch ? LEXTYPE_NAME_SW_TOGGLE_ON : LEXTYPE_NAME_TOGGLE_ON);
    ndef.name = namebuf + 2;
    ndef.namelen -= 2;
    if (onpp != 0) {
        onp = name_declare(scope, &ndef, 0, 0, 0, &datap);
        if (onp == 0) {
            if (offp != 0) name_free(offp);
            return 0;
        }
        datap->handler = handler;
        datap->ctx = ctx;
        datap->togindex = togidx;
        *onpp = onp;
    }
    return 1;

} /* toggle_declare */

int switch_toggle_declare (scopectx_t scope, strdesc_t *name, toggle_handler_fn handler,
                           void *ctx, int togidx, name_t **onpp, name_t **offpp) {
    return toggle_declare(scope, 1, name, handler, ctx, togidx, onpp, offpp);
}
int value_toggle_declare (scopectx_t scope, strdesc_t *name, toggle_handler_fn handler,
                           void *ctx, int togidx, name_t **onpp, name_t **offpp) {
    return toggle_declare(scope, 0, name, handler, ctx, togidx, onpp, offpp);
}

/*
 * value_toggle_dispatch
 *
 * Dispatch the handler for a value toggle.
 */
int
value_toggle_dispatch (parse_ctx_t pctx, void *ctx, lextype_t dcltype, name_t *togname)
{
    struct toggle_s *t = name_extraspace(togname);

    if (name_type(togname) != LEXTYPE_NAME_TOGGLE_OFF &&
        name_type(togname) != LEXTYPE_NAME_TOGGLE_ON) {
        return 0;
    }

    return (*t->handler)(pctx, ctx, t->togindex, dcltype, togname);

} /* value_toggle_dispatch */

/*
 * parse_switches
 *
 * Parses SWITCHES declarations and MODULE switches.
 *
 */
int
parse_switches (parse_ctx_t pctx, lextype_t decltype, lextype_t terminator)
{
    int i;
    lexeme_t *lex;
    name_t *np;

    while (1) {
        i = parser_expect_oneof(pctx, QL_NORMAL,
                                switch_types, sizeof(switch_types)/sizeof(switch_types[0]),
                                &lex, 1);
        if (i < 0) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__SWITCHEXP);
            break;
        }
        np = lexeme_ctx_get(lex);
        if (np == 0) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__SWITCHUNS,
                       lexeme_text(lex));
        } else {
            lextype_t lt;
            lt = lexeme_type(lex);
            if (lt == LEXTYPE_NAME_SW_TOGGLE_OFF || lt == LEXTYPE_NAME_SW_TOGGLE_ON) {
                struct toggle_s *t = name_extraspace(np);
                if (t->handler == 0) {
                    log_signal(parser_logctx(pctx), parser_curpos(pctx),
                               STC__SWITCHUNS, lexeme_text(lex));
                } else {
                    (*t->handler)(pctx, t->ctx, t->togindex, decltype, np);
                }
            } else {
                struct switch_special_s *s = name_extraspace(np);
                if (s->handler == 0) {
                    log_signal(parser_logctx(pctx), parser_curpos(pctx),
                               STC__SWITCHUNS, lexeme_text(lex));
                } else {
                    (*s->handler)(pctx, s->ctx, decltype, lex);
                }
            }
        }
        lexeme_free(parser_lexmemctx(pctx), lex);
        if (parser_expect(pctx, QL_NORMAL, terminator, 0, 1)) {
            return 1;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__DELIMEXP, ",");
        }

    } /* while 1 */

    // If we broke out of the loop, there was some kind of error, try to recover
    parser_skip_to_delim(pctx, terminator);
    return 1;

} /* parse_switches */
