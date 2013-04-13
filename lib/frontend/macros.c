/*
 *++
 * macros.c - Macro declarations and expansion.
 *
 * This module handles the processing of macro declarations
 * and expansion.
 *
 * Macro and macro parameter names are managed here, using
 * the extensions provided by the nametable module.
 *
 * Some of the facilities here are used by the structures
 * module, since structures are implemented essentially as
 * simple macros.  In particular, structures use the MAC_PARAM
 * name type for structure parameters, and those get parsed
 * in the normal way, unlike real macro parameters, which
 * get stashed in a private table and bound manually during
 * the prepare_body() phase of macro expansion.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdlib.h>
#include <stdio.h>
#include "blissc/declarations.h"
#include "blissc/parser.h"
#include "blissc/expression.h"
#include "blissc/lexer.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"
#include "blissc/support/strings.h"
#include "blissc/macros.h"

// Macro structure
typedef enum {
    MACRO_UNK, MACRO_SIMPLE, MACRO_COND, MACRO_ITER, MACRO_KWD
} macrotype_t;

struct macrodecl_s {
    lexseq_t body;
    macrotype_t type;
    namereflist_t plist, ilist;
    scopectx_t ptable;
};

struct expansion_s {
    struct expansion_s  *next;
    name_t              *curmacro;
    scopectx_t           expscope;
    lexseq_t             remaining;
    long                 count;
    long                 nactuals;
    lexeme_t            *sep, *closer;
};
typedef struct expansion_s expansion_t;

typedef enum { EXP_NORMAL, EXP_EXITITER, EXP_EXITMACRO, EXP_ERRORMACRO } expstate_t;

struct macroctx_s {
    expr_ctx_t           ectx;
    expstate_t           state;
    expansion_t         *curexp;
    expansion_t         *freeexps;
};

static namedef_t macro_names[] = {
    NAMEDEF("KEYWORDMACRO", LEXTYPE_DCL_KEYWORDMACRO, NAME_M_RESERVED),
    NAMEDEF("MACRO", LEXTYPE_DCL_MACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITMACRO", LEXTYPE_LXF_EXITMACRO, NAME_M_RESERVED),
    NAMEDEF("%EXITITERATION", LEXTYPE_LXF_EXITITER, NAME_M_RESERVED),
    NAMEDEF("%ERRORMACRO", LEXTYPE_LXF_ERRORMACRO, NAME_M_RESERVED),
    NAMEDEF("%REMAINING", LEXTYPE_LXF_REMAINING, NAME_M_RESERVED),
    NAMEDEF("%COUNT", LEXTYPE_LXF_COUNT, NAME_M_RESERVED),
    NAMEDEF("%LENGTH", LEXTYPE_LXF_LENGTH, NAME_M_RESERVED)
};

static namedef_t predeclared_macros[] = {
    NAMEDEF("%BLISSM",  LEXTYPE_NAME_MACRO, 0),
    NAMEDEF("%BLISS16", LEXTYPE_NAME_MACRO, 0),
    NAMEDEF("%BLISS32", LEXTYPE_NAME_MACRO, 0),
    NAMEDEF("%BLISS36", LEXTYPE_NAME_MACRO, 0)
};

static lextype_t openers[] = { LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_LBRACK,
                               LEXTYPE_DELIM_LANGLE };
static lextype_t closers[] = { LEXTYPE_DELIM_RPAR, LEXTYPE_DELIM_RBRACK,
                               LEXTYPE_DELIM_RANGLE };

static int prepare_body(macroctx_t mctx, expansion_t *curexp, lexseq_t *result);
static int macro_expand(macroctx_t ctx, name_t *macroname,
                        lexseq_t *result);

/*
 * expansion_alloc
 *
 * Allocate an expansion context.
 */
static expansion_t *
expansion_alloc (macroctx_t mctx)
{
    expansion_t *e;
    if (mctx->freeexps == 0) {
        e = malloc(sizeof(expansion_t));
    } else {
        e = mctx->freeexps;
        mctx->freeexps = e->next;
    }
    if (e != 0) {
        memset(e, 0, sizeof(expansion_t));
    }

    return e;

} /* expansion_alloc */

/*
 * expansion_free
 *
 * Free an expansion context.
 */
void
expansion_free (macroctx_t mctx, expansion_t *e)
{
    lexctx_t lctx = expr_lexmemctx(mctx->ectx);

    lexeme_free(lctx, e->sep);
    lexseq_free(lctx, &e->remaining);
    if (e->expscope != 0) {
        scope_end(e->expscope);
    }
    memset(e, 0x29, sizeof(expansion_t));
    e->next = mctx->freeexps;
    mctx->freeexps = e;

} /* expansion_free */
/*
 * macro_bind
 *
 * Lexical binding for macros.  Invoked via lexeme_bind when a
 * macro name is encountered.
 */
static int
macro_bind (lexctx_t lctx, void *vctx, quotelevel_t ql, quotemodifier_t qm,
            lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    macroctx_t ctx = vctx;
    name_t *np = lexeme_ctx_get(lex);

    if (cs == COND_CWA || cs == COND_AWC || ql == QL_MACROSKIP) {
        lexeme_free(lctx, lex);
        return 1;
    }

    switch (qm) {
        case QM_QUOTE:
            lex->type = LEXTYPE_UNBOUND;
            return 0;
        case QM_EXPAND:
            lexeme_free(lctx, lex);
            return macro_expand(ctx, np, result);
        case QM_UNQUOTE:
            lex->type = LEXTYPE_NAME_MACRO;
            return 0;
        default:
            break;
    }

    // no quote modifier

    if (ql == QL_MACRO) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexeme_free(lctx, lex);
    return macro_expand(ctx, np, result);

} /* macro_bind */

/*
 * macroend_bind
 *
 * Lexical binding for the special MACROEND lexeme type.  No quotelevel/quotemodifier/cond
 * checks here.
 */
static int
macroend_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    macroctx_t mctx = ctx;
    expansion_t *cur = mctx->curexp;
    expr_ctx_t ectx = mctx->ectx;
    parse_ctx_t pctx = expr_parse_ctx(ectx);
    struct macrodecl_s *curmac;

    lexeme_free(lctx, lex);

    if (cur == 0) {
        expr_signal(mctx->ectx, STC__INTCMPERR, "macroend_bind");
        return 1;
    }

    curmac = name_extraspace(cur->curmacro);

    switch (mctx->state) {
        case EXP_EXITMACRO:
            lexseq_free(lctx, &cur->remaining);
            // FALLTHROUGH
        case EXP_EXITITER:
        case EXP_NORMAL:
            if (curmac->type == MACRO_ITER && lexseq_length(&cur->remaining) > 0) {
                if (cur->sep != 0) {
                    lexseq_instail(result, lexeme_copy(lctx, cur->sep));
                }
                mctx->state = EXP_NORMAL;
                parser_skipmode_set(pctx, 0);
                return prepare_body(mctx, cur, result);
            }
            if (curmac->type == MACRO_ITER && cur->closer != 0) {
                lexseq_instail(result, cur->closer);
            }
            // FALLTHROUGH
        case EXP_ERRORMACRO:
            mctx->curexp = cur->next;
            expansion_free(mctx, cur);
            if (mctx->state != EXP_ERRORMACRO || mctx->curexp == 0) {
                parser_skipmode_set(pctx, 0);
            }
            break;
    }

    mctx->state = EXP_NORMAL;
    return 1;

} /* macroend_bind */
/*
 * macparam_bind
 *
 * Lexical binding for macro parameters.  Invoked via lexeme_bind
 * when a macro parameter name is encountered.
 */
static int
macparam_bind (lexctx_t lctx, void *ctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *lex, lexseq_t *result)
{
    name_t *np = lexeme_ctx_get(lex);
    lexseq_t tmpseq;

    if (cs == COND_CWA || cs == COND_AWC || ql == QL_MACROSKIP) {
        lexeme_free(lctx, lex);
        return 1;
    }
    if (qm == QM_QUOTE) {
        lex->type = LEXTYPE_UNBOUND;
        return 0;
    }

    lexseq_init(&tmpseq);
    lexseq_copy(lctx, &tmpseq, name_extraspace(np));
    lexseq_append(result, &tmpseq);
    lexeme_free(lctx, lex);

    return 1;

} /* macparam_bind */

/*
 * Macro parameter accessors and constructor/destructors
 */

/*
 * macparam_lexseq
 *
 * Returns the pointer to the lexeme sequence held by
 * the macro parameter.
 */
lexseq_t *
macparam_lexseq (name_t *np)
{
    return name_extraspace(np);

} /* macparam_lexseq */

/*
 * macparam_initdata
 *
 * Constructor for macro parameters.
 */
static int
macparam_initdata (void *vctx, name_t *np, void *p)
{
    lexseq_t *seq = p;
    lexseq_init(seq);
    return 1;

} /* macparam_initdata */

/*
 * macparam_freedata
 *
 * Destructor for macro parameters.
 */
static void
macparam_freedata (void *vctx, name_t *np, void *p)
{
    lexctx_t lctx = expr_lexmemctx(vctx);
    lexseq_t *seq = p;
    lexseq_free(lctx, seq);

} /* macparam_freedata */

/*
 * macparam_copydata
 *
 * Copy-constructor for macro parameters.
 */
static int
macparam_copydata (void *vctx, name_t *dst, void *dp, name_t *src, void *sp)
{
    lexctx_t lctx = expr_lexmemctx(vctx);
    lexseq_free(lctx, dp);
    lexseq_copy(lctx, dp, sp);
    return 1;

} /* macparam_copydata */

/*
 * macparam_serialize
 */
static int
macparam_serialize (void *vctx, name_t *np, void *fh)
{
    if (name_serialize(np, fh, 0, 0)) {
        return lexseq_serialize(fh, name_extraspace(np));
    }
    return 0;

} /* macparam_serialize */

/*
 * Macro constructors/desctructors
 */

/*
 * macro_initdata
 *
 * Macro constructor.
 */
static int
macro_initdata (void *vctx, name_t *np, void *p)
{
    struct macrodecl_s *macro = p;

    namereflist_init(&macro->plist);
    namereflist_init(&macro->ilist);
    macro->ptable = 0;
    lexseq_init(&macro->body);
    return 1;

} /* macro_initdata */

/*
 * macro_freedata
 *
 * Macro destructor.
 */
static void
macro_freedata (void *vctx, name_t *np, void *p)
{
    struct macrodecl_s *macro = p;
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);

    namereflist_free(namectx, &macro->plist);
    namereflist_free(namectx, &macro->ilist);
    lexseq_free(lctx, &macro->body);
    scope_end(macro->ptable);

} /* macro_freedata */

/*
 * macro_copydata
 *
 * Copy-constructor for macros.
 */
static int
macro_copydata (void *vctx, name_t *dst, void *dp, name_t *src, void *sp)
{
    struct macrodecl_s *srcm = sp;
    struct macrodecl_s *dstm = dp;
    nameref_t *ref;
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);

    lexseq_init(&dstm->body);
    lexseq_copy(lctx, &dstm->body, &srcm->body);
    dstm->ptable = scope_copy(srcm->ptable, 0);
    namereflist_init(&dstm->plist);
    namereflist_init(&dstm->ilist);
    dstm->type = srcm->type;
    // We can't just copy the namereflists over verbatim; the copied
    // list has to reference the names in the copied name table, not
    // the original.
    for (ref = namereflist_head(&srcm->plist); ref != 0; ref = ref->tq_next) {
        name_t *np;
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *namestr = name_string(ref->np);
            np = name_search(dstm->ptable, namestr->ptr, namestr->len, 0);
        }
        namereflist_instail(&dstm->plist, nameref_alloc(namectx, np));
    }
    for (ref = namereflist_head(&srcm->ilist); ref != 0; ref = ref->tq_next) {
        name_t *np;
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *namestr = name_string(ref->np);
            np = name_search(dstm->ptable, namestr->ptr, namestr->len, 0);
        }
        namereflist_instail(&dstm->ilist, nameref_alloc(namectx, np));
    }

    return 1;

} /* macro_copydata */

/*
 * macro_serialize
 */
static int
macro_serialize (void *vctx, name_t *np, void *fh)
{
    struct macrodecl_s *m = name_extraspace(np);
    uint16_t buf[4];
    int status;

    buf[0] = (uint16_t)m->type;
    buf[1] = (uint16_t)namereflist_length(&m->plist);
    buf[2] = (uint16_t)namereflist_length(&m->ilist);
    buf[3] = 0;
    status = name_serialize(np, fh, buf, sizeof(buf));
    if (status) status = scope_serialize(m->ptable, fh);
    if (status) status = namereflist_serialize(&m->plist, fh);
    if (status) status = namereflist_serialize(&m->ilist, fh);
    if (status) status = lexseq_serialize(fh, &m->body);

    return status;

} /* macro_serialize */

/*
 * handle_specials
 */
int
handle_specials (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    macroctx_t ctx = vctx;
    strdesc_t *str;
    lexseq_t result;

    if (ctx->curexp == 0) {
        log_signal(parser_logctx(pctx), parser_curpos(pctx), STC__INVMACFUN);
        return 1;
    }
    switch (curlt) {
        case LEXTYPE_LXF_COUNT:
            // our internal counter is 1-based; %COUNT is zero-based.
            str = string_printf(parser_strctx(pctx), 0, "%ld", ctx->curexp->count-1);
            parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str));
            string_free(parser_strctx(pctx), str);
            break;
        case LEXTYPE_LXF_LENGTH:
            str = string_printf(parser_strctx(pctx), 0, "%ld", ctx->curexp->nactuals);
            parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str));
            string_free(parser_strctx(pctx), str);
            break;
        case LEXTYPE_LXF_REMAINING:
            lexseq_init(&result);
            lexseq_copy(parser_lexmemctx(pctx), &result, &ctx->curexp->remaining);
            parser_insert_seq(pctx, &result);
            break;
        default:
            break;
    }

    return 1;

} /* handle_specials */

/*
 * handle_exits
 */
int
handle_exits (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    macroctx_t ctx = vctx;
    expansion_t *cur = ctx->curexp;
    struct macrodecl_s *macro;
    lexeme_t *lex;

    if (ctx->curexp == 0) {
        expr_signal(ctx->ectx, STC__INTCMPERR, "handle_exits[1]");
        return 1;
    }

    if (ctx->state != EXP_NORMAL) {
        return 1;
    }

    macro = name_extraspace(cur->curmacro);

    switch (curlt) {
        case LEXTYPE_LXF_EXITITER:
            if (macro->type != MACRO_ITER) {
                expr_signal(ctx->ectx, STC__INVEXITER, name_string(cur->curmacro));
                ctx->state = EXP_EXITMACRO;
            }
            parser_skipmode_set(pctx, 1);
            ctx->state = EXP_EXITITER;
            break;
        case LEXTYPE_LXF_EXITMACRO:
            parser_skipmode_set(pctx, 1);
            ctx->state = EXP_EXITMACRO;
            break;
        case LEXTYPE_LXF_ERRORMACRO:
            lex = parse_string_params(pctx, 0);
            if (lex == 0) {
                expr_signal(ctx->ectx, STC__SYNTAXERR);
            } else {
                expr_signal(ctx->ectx, STC__USRERR, lexeme_text(lex));
                lexeme_free(expr_lexmemctx(ctx->ectx), lex);
            }
            parser_skipmode_set(pctx, 1);
            ctx->state = EXP_ERRORMACRO;
            break;
        default:
            expr_signal(ctx->ectx, STC__INTCMPERR, "handle_exits[2]");
            break;
    }

    return 1;

} /* handle_exits */

/*
 * macros_init
 *
 * Module initialization routine.
 */
macroctx_t
macros_init (scopectx_t kwdscope, expr_ctx_t ctx)
{
    namectx_t namectx = scope_namectx(kwdscope);
    lexctx_t lctx = expr_lexmemctx(ctx);
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    macroctx_t mctx = malloc(sizeof(struct macroctx_s));
    nametype_vectors_t vec;
    int i;

    for (i = 0; i < sizeof(macro_names)/sizeof(macro_names[0]); i++) {
        name_declare(kwdscope, &macro_names[i], 0, 0, 0, 0);
    }

    memset(mctx, 0, sizeof(struct macroctx_s));
    mctx->ectx = ctx;
    mctx->state = EXP_NORMAL;
    lextype_register(lctx, mctx, LEXTYPE_NAME_MACRO, macro_bind);
    lextype_register(lctx, ctx, LEXTYPE_NAME_MAC_PARAM, macparam_bind);
    lextype_register(lctx, mctx, LEXTYPE_MACROEND, macroend_bind);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_REMAINING, handle_specials);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_COUNT, handle_specials);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_LENGTH, handle_specials);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_EXITMACRO, handle_exits);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_EXITITER, handle_exits);
    parser_lexfunc_register(pctx, mctx, LEXTYPE_LXF_ERRORMACRO, handle_exits);
    memset(&vec, 0, sizeof(vec));
    vec.typesize = sizeof(struct macrodecl_s);
    vec.typeinit = macro_initdata;
    vec.typefree = macro_freedata;
    vec.typecopy = macro_copydata;
    vec.typeser  = macro_serialize;
    nametype_dataop_register(namectx, LEXTYPE_NAME_MACRO, &vec, ctx);
    memset(&vec, 0, sizeof(vec));
    vec.typesize = sizeof(lexseq_t);
    vec.typeinit = macparam_initdata;
    vec.typefree = macparam_freedata;
    vec.typecopy = macparam_copydata;
    vec.typeser  = macparam_serialize;
    nametype_dataop_register(namectx, LEXTYPE_NAME_MAC_PARAM, &vec, ctx);

    for (i = 0; i < sizeof(predeclared_macros)/sizeof(predeclared_macros[0]); i++) {
        name_t *np;
        struct macrodecl_s *m;

        np = name_declare(kwdscope, &predeclared_macros[i], 0, 0, 0, &m);
        if (np == 0 || m == 0) {
            expr_signal(ctx, STC__INTCMPERR, "macros_init");
            continue;
        }
        m->type = MACRO_COND;
        // Index 0 is the %BLISSM[] macro
        if (i == 0) {
            strdesc_t pctremain = STRDEF("%REMAINING");
            lexeme_t *lex = lexeme_create(lctx, LEXTYPE_LXF_REMAINING, &pctremain);
            lexseq_instail(&m->body, lex);
        }
        // All other macro bodies are empty
    }

    return mctx;

} /* macros_init */

/*
 * macros_finish
 *
 * Shutdown for this module.
 */
void
macros_finish (macroctx_t mctx)
{
    expansion_t *e, *enext;

    if (mctx == 0) {
        return;
    }
    // Clean up any current expansions
    for (e = mctx->curexp; e != 0; e = enext) {
        enext = e->next;
        expansion_free(mctx, e);
    }
    // Now release the memory
    for (e = mctx->freeexps; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }
    free(mctx);

} /* macros_finish */

/*
 * macparam_special
 *
 * Declares a macro parameter name.  It's 'special' because it bypasses
 * the normal existence checks.
 */
name_t *
macparam_special (scopectx_t scope, strdesc_t *pname, lexseq_t *seqval)
{
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MAC_PARAM;
    ndef.name = pname->ptr;
    ndef.namelen = pname->len;

    return name_declare_nocheck(scope, &ndef, 0, seqval, sizeof(lexseq_t), 0);

} /* macparam_special */

/*
 * macparam_lookup
 *
 * Looks up a macro parameter and optionally returns a copy of its lexeme
 * sequence value.  Only succeeds if the name is defined in the specified scope.
 */
name_t *
macparam_lookup (lexctx_t lctx, scopectx_t scope, strdesc_t *pname, lexseq_t *value)
{
    name_t *np;
    lexseq_t *seq;

    np = name_search_typed(scope, pname->ptr, pname->len,
                           LEXTYPE_NAME_MAC_PARAM, &seq);
    if (np != 0 && name_scope(np) == scope && value != 0) {
        lexseq_copy(lctx, value, seq);
    }

    return np;

} /* macparam_lookup */

/*
 * macro_paramlist
 *
 * Parse a macro or structure parameter list, for declarations.
 *
 * For keyword macros and structure declarations,
 * handles the default value setting.
 *
 * For non-keyword macros, handles simple, conditional, and
 * iterative macro parameter lists.
 *
 * Returns: -1 on error
 * index of closing delimiter in delims[] array on success
 */
int
macro_paramlist (expr_ctx_t ctx, scopectx_t curscope,
                 int assign_allowed, int for_macro,
                 lextype_t delims[], int ndelims,
                 scopectx_t *ptable, namereflist_t *plist)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = scope_namectx(parser_scope_get(pctx));
    lexctx_t lctx = parser_lexmemctx(pctx);
    lexeme_t *lex;
    lextype_t lt;
    name_t *mnp;
    scopectx_t pscope;
    lexseq_t nullseq;
    namedef_t ndef;
    int i, did1;
    lextype_t terms[16];

    // Need a delimiter array that adds ',' to the
    // caller's list
    if (ndelims >= 16) return -1;
    memcpy(terms, delims, sizeof(lextype_t)*ndelims);
    terms[ndelims] = LEXTYPE_DELIM_COMMA;

    if (*ptable == 0) {
        pscope = scope_begin(scope_namectx(curscope), curscope);
    } else {
        pscope = *ptable;
    }

    lexseq_init(&nullseq);
    parser_scope_begin(pctx);
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MAC_PARAM;
    did1 = 0;

    while (1) {
        lexseq_t *pseq;
        strdesc_t *namestr;
        textpos_t pos;
        // Look for a parameter name
        if (!parse_decl_name(pctx, &namestr, &pos)) {
            // Null parameter list is OK, but empty parameter
            // after a comma is not
            if (did1) {
                expr_signal(ctx, STC__NAMEEXP);
            }
            lt = parser_next(pctx, QL_NAME, 0);
            break;
        }

        // Declare the parameter in the parameter name table
        did1 = 1;
        ndef.name = namestr->ptr;
        ndef.namelen = namestr->len;
        mnp = name_declare(pscope, &ndef, pos, 0, 0, &pseq);
        string_free(expr_strctx(ctx), namestr);
        if (mnp == 0) {
            expr_signal(ctx, STC__INTCMPERR, "macro_paramlist");
            break;
        }
        namereflist_instail(plist, nameref_alloc(namectx, mnp));
        // If assignment is allowed, parse the '=' and the
        // default value - a lexeme sequence for macros, a CTCE for
        // structures
        lt = parser_next(pctx, QL_NAME, &lex);
        if (assign_allowed && lt == LEXTYPE_OP_ASSIGN) {
            int status;
            lexeme_free(lctx, lex);
            if (for_macro) {
                status = parse_lexeme_seq(pctx, 0, QL_MACRO,
                                          terms, ndelims+1, pseq, &lt);
            } else {
                status = expr_parse_ctce(ctx, &lex, 0);
                if (status) lexseq_instail(pseq, lex);
                lt = parser_next(pctx, QL_NORMAL, 0);
            }
            if (!status) {
                expr_signal(ctx, STC__SYNTAXERR);
                break;
            }
        } else {
            lexeme_free(lctx, lex);
        }
        if (lt != LEXTYPE_DELIM_COMMA) {
            break;
        }
    }
    parser_scope_end(pctx);

    // Check for the closing delimiter
    for (i = 0; i < ndelims && lt != delims[i]; i++);
    if (i >= ndelims) {
        scope_end(pscope);
        parser_skip_to_delim(pctx, delims[0]);
        namereflist_free(namectx, plist);
        i = -1;
    } else if (*ptable == 0) {
        *ptable = pscope;
    }

    return i;

} /* macro_paramlist */

/*
 * declare_macro
 *
 * Common logic for parsing macro declarations.
 *
 * MACRO macro-name { (param,...) } { [param,...] } = {stuff} % {,...}
 * KEYWORDMACRO macro-name { (param{=defval},...) } = {stuff} % {,...}
 */
int
declare_macro (expr_ctx_t ctx, scopectx_t scope, lextype_t curlt)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    int skip_to_end = 0;
    lexeme_t *lex;
    lextype_t lt;
    strdesc_t *ltext;
    name_t *np;
    int is_kwdmacro = (curlt == LEXTYPE_DCL_KEYWORDMACRO);
    struct macrodecl_s *macro;
    textpos_t pos;
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MACRO;
    ndef.flags = NAME_M_DECLARED;
    while (1) {
        if (!parse_decl_name(pctx, &ltext, &pos)) {
            expr_signal(ctx, STC__NAMEEXP);
            skip_to_end = 1;
        }
        lt = parser_next(pctx, QL_NAME, 0);
        if (lt != LEXTYPE_DELIM_LPAR &&
            lt != LEXTYPE_OP_ASSIGN &&
            lt != LEXTYPE_DELIM_LBRACK) {
            expr_signal(ctx, STC__SYNTAXERR);
            skip_to_end = 1;
        }
        ndef.name = ltext->ptr;
        ndef.namelen = ltext->len;
        np = name_declare(scope, &ndef, pos, 0, 0, &macro);
        macro->type = (is_kwdmacro ? MACRO_KWD : MACRO_UNK);
        // Parse the regular parameters
        if (lt == LEXTYPE_DELIM_LPAR) {
            if (macro_paramlist(ctx, scope, is_kwdmacro, 1,
                                closers, 1, &macro->ptable,
                                &macro->plist) < 0) {
                skip_to_end = 1;
            }
            if (namereflist_length(&macro->plist) == 0) {
                expr_signal(ctx, STC__NOMACPRMS);
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            if (!is_kwdmacro) {
                macro->type = MACRO_SIMPLE;
            }
        }
        // Parse the iterative parameters (or the empty bracket pair
        // for conditional macros)
        if (lt == LEXTYPE_DELIM_LBRACK) {
            if (is_kwdmacro) {
                expr_signal(ctx, STC__ITERKWMAC);
                skip_to_end = 1;
            }
            if (macro_paramlist(ctx, scope, 0, 1, &closers[1], 1,
                                &macro->ptable, &macro->ilist) < 0) {
                skip_to_end = 1;
            }
            lt = parser_next(pctx, QL_NAME, 0);
            macro->type = (namereflist_length(&macro->ilist) == 0 ?
                           MACRO_COND : MACRO_ITER);
        }

        if (lt == LEXTYPE_OP_ASSIGN) {

            // If the type is still unknown, there were no parameter
            // lists, so we know this is a simple, non-parameterized
            // macro
            if (macro->type == MACRO_UNK) {
                macro->type = MACRO_SIMPLE;
            }

            // Parse the macro body.  It must be wholly contained within
            // the currently open file, so we set ERRONEOF to tell the
            // parser that.
            parser_incr_erroneof(pctx);
            parser_scope_begin(pctx);
            for (lt = parser_next(pctx, QL_MACRO, &lex);
                 lt != LEXTYPE_LXF_DELIM_PERCENT;
                 lt = parser_next(pctx, QL_MACRO, &lex)) {
                if (lt == LEXTYPE_END || lt == LEXTYPE_NONE) {
                    expr_signal(ctx, STC__SYNTAXERR);
                    skip_to_end = 1;
                    break;
                }
                lexseq_instail(&macro->body, lex);
            }
            parser_scope_end(pctx);
            parser_decr_erroneof(pctx);
        }

        if (skip_to_end) {
            name_free(np);
            string_free(expr_strctx(ctx), ltext);
            break;
        }
        if (macro->ptable != 0) {
            scope_setparent(macro->ptable, 0);
        }
        string_free(expr_strctx(ctx), ltext);

        if (parser_expect(pctx, QL_NAME, LEXTYPE_DELIM_SEMI, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NAME, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_SEMI);
            break;
        }

    } /* while 1 */

    return 1;

} /* define_macro */

/*
 * prepare_body
 *
 * Builds a lexeme sequence with the expansion of the body of
 * a macro.
 */
static int
prepare_body (macroctx_t mctx, expansion_t *curexp, lexseq_t *result)
{
    expr_ctx_t ctx = mctx->ectx;
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = parser_lexmemctx(pctx);
    struct macrodecl_s *macro = name_extraspace(curexp->curmacro);
    lexeme_t *lex;
    name_t *np;
    static strdesc_t mend = STRDEF("<macro-end>");

    // Prepare the grouper and separator lexemes, in case
    // they are needed (only applicable to iterative macros)
    if (curexp->count == 0 && macro->type == MACRO_ITER) {
        curexp->sep = parser_punct_separator(pctx);
        lex = parser_punct_grouper(pctx, 0);
        if (lex != 0) {
            lexseq_instail(result, lex);
        }
        curexp->closer = parser_punct_grouper(pctx, 1);
    }

    // Associate the iterative-formals with any actuals
    if (macro->type == MACRO_ITER) {
        nameref_t *iformal;
        lextype_t lt, terms[1] = { LEXTYPE_DELIM_COMMA };

        for (iformal = namereflist_head(&macro->ilist); iformal != 0;
             iformal = iformal->tq_next) {
            strdesc_t *namestr = name_string(iformal->np);
            np = macparam_special(curexp->expscope, namestr, 0);
            parse_lexeme_seq(pctx, &curexp->remaining, QL_MACRO, terms, 1,
                             macparam_lexseq(np), &lt);
        }
    }

    // Expand the macro parameters here, instead of relying on the parser
    // to do this while processing the expansion.  If we were to wait, we'd
    // have to push a new scope on the parser's stack and remove it when we're
    // done -- but the expansion could declare other names, and those would
    // get declared in the scope we're removing.
    for (lex = lexseq_head(&macro->body); lex != 0; lex = lexeme_next(lex)) {
        if (curexp->expscope != 0 && lexeme_boundtype(lex) == LEXTYPE_NAME) {
            lexseq_t seq;
            lexseq_init(&seq);
            if (macparam_lookup(lctx, curexp->expscope, lexeme_text(lex), &seq) != 0) {
                lexseq_append(result, &seq);
            } else {
                lexseq_instail(result, lexeme_copy(lctx, lex));
            }
        } else {
            lexseq_instail(result, lexeme_copy(lctx, lex));
        }
    }
    lexseq_instail(result, lexeme_create(lctx, LEXTYPE_MACROEND, &mend));
    curexp->count += 1;

    return 1;

} /* prepare_body */

/*
 * macro_expand
 *
 * Expands a macro.
 */
static int
macro_expand (macroctx_t mctx, name_t *macroname, lexseq_t *result)
{
    struct macrodecl_s *macro = name_extraspace(macroname);
    expr_ctx_t ctx = mctx->ectx;
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = parser_lexmemctx(pctx);
    expansion_t *curexp = expansion_alloc(mctx);
    expansion_t *prev_exp;
    lextype_t lt;
    lexeme_t *lex;
    lexseq_t extras;
    name_t *np;
    scopectx_t expscope;
    int which;
    int nactuals;
    punctclass_t pcl;
    lextype_t psep;
    lextype_t terms[3];
    static strdesc_t comma = STRDEF(",");

    if (macro == 0 || curexp == 0) {
        expr_signal(ctx, STC__INTCMPERR, "macro_expand");
        return 1;
    }
    memset(curexp, 0, sizeof(struct expansion_s));

    // We save the punctuation class here, since it will get
    // munged by the parsing of the macro parameters.
    parser_punctclass_get(pctx, &pcl, &psep);

    prev_exp = 0;
    if (macro->type == MACRO_COND) {
        for (prev_exp = mctx->curexp; prev_exp != 0 && prev_exp->curmacro != macroname;
             prev_exp = prev_exp->next);
    }

    nactuals = 0;
    lexseq_init(&extras);

    // Simple macros with no formal arguments get no processing
    // of parameter lists whatsoever.
    if (macro->type == MACRO_SIMPLE && namereflist_length(&macro->plist) == 0) {
        expscope = 0;
        which = 3;
    } else {
        // For keyword macros, prime the scope with the declared
        // formals, so we can inherit the default values.
        if (macro->type == MACRO_KWD) {
            expscope = scope_copy(macro->ptable, 0);
        } else {
            expscope = scope_begin(scope_namectx(parser_scope_get(pctx)), 0);
        }

        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (macro->type == MACRO_KWD) {
            if (lt != LEXTYPE_DELIM_LPAR) {
                expr_signal(ctx, STC__DELIMEXP, "(");
                parser_insert(pctx, lex);
                return 1;
            }
            which = 0;
            lexeme_free(lctx, lex);
        } else {
            for (which = 0; lt != openers[which] && which < 3; which++);
            if (which >= 3 && namereflist_length(&macro->plist) > 0) {
                expr_signal(ctx, STC__DELIMEXP, "(");
                parser_insert(pctx, lex);
                return 1;
            }
            if (which >= 3) {
                parser_insert(pctx, lex);
            } else {
                lexeme_free(lctx, lex);
            }
        }
    }

    // If we had a match on an opener, process
    // the actual parameters.
    if (which < 3) {

        nameref_t *formal;
        lexseq_t val;

        terms[0] = LEXTYPE_DELIM_COMMA;
        terms[1] = closers[which];

        formal = namereflist_head(&macro->plist);

        while (1) {
            // Keyword macro actuals are of the form name=value
            // For positionals, grab the next formal-parameter name,
            // or set np to NULL to add the actual to %REMAINING.
            if (macro->type == MACRO_KWD) {
                lt = parser_next(pctx, QL_NAME, &lex);
                if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
                    expr_signal(ctx, STC__NAMEEXP);
                    lexeme_free(lctx, lex);
                    break;
                }
                np = name_search(macro->ptable, lex->text.ptr, lex->text.len, 0);
                if (np == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "macro_expand[2]");
                }
                lexeme_free(lctx, lex);
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
                    expr_signal(ctx, STC__OPEREXP, "=");
                    break;
                }
            } else if (nactuals < namereflist_length(&macro->plist)) {
                np = formal->np;
                formal = formal->tq_next;
            } else {
                np = 0;
            }
            lexseq_init(&val);
            // Now parse the actual-parameter, which can be an expression
            if (!parse_lexeme_seq(pctx, 0, QL_NAME, terms, 2, &val, &lt)) {
                lexseq_free(lctx, &val);
                break;
            }

            // If we are recursively expanding a conditional macro and
            // there are no parameters, we're done - no expansion.
            if (prev_exp != 0 && lexseq_length(&val) == 0 && nactuals == 0) {
                scope_end(expscope);
                free(curexp);
                return 1;
            }

            if (np == 0) {
                if (lexseq_length(&extras) > 0) {
                    lexseq_instail(&extras,
                                   lexeme_create(lctx, LEXTYPE_DELIM_COMMA, &comma));
                }
                lexseq_append(&extras, &val);
            } else {
                name_t *actual;
                // Associate the actual with the formal.  For keyword
                // macros, the scope_copy() above sets a special "no check"
                // flag that allows each name to be redeclared once.
                // name_declare() clears this flag, so we can catch genuine
                // redeclarations.
                actual = macparam_special(expscope, name_string(np), &val);
                if (actual == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "macro_expand[3]");
                }
                lexseq_free(lctx, &val);
            }

            nactuals += 1;

            if (lt == closers[which]) {
                break;
            }

            if (lt != LEXTYPE_DELIM_COMMA) {
                expr_signal(ctx, STC__DELIMEXP, ",");
                break;
            }

        } /* while (1) */

        if (lt != closers[which]) {
            expr_signal(ctx, STC__DELIMEXP, "closer");
            lexseq_free(lctx, &extras);
            scope_end(expscope);
            return 1;
        }

        if (nactuals < namereflist_length(&macro->plist)) {
            name_t *anp;
            while (formal != 0) {
                anp = macparam_special(expscope, name_string(formal->np), 0);
                if (anp == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "macro_expand[4]");
                }
                formal = formal->tq_next;
            }
        }

    } /* if which < 3 */

    // The macro actual parameters are now processed; hook
    // the scope into the current hierarchy, restore the punctuation
    // class to what it was before we parsed the actuals, and
    // generate the expansion sequence.

    parser_punctclass_set(pctx, pcl, psep);

    curexp->count = (prev_exp == 0 ? 0 : prev_exp->count);
    curexp->expscope = expscope;
    curexp->curmacro = macroname;
    curexp->next = mctx->curexp;
    curexp->nactuals = nactuals;
    lexseq_append(&curexp->remaining, &extras);
    mctx->curexp = curexp;

    return prepare_body(mctx, curexp, result);

} /* macro_expand */
