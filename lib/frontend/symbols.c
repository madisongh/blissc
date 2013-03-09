/*
 *++
 * symbols.c - Symbol management.
 *
 * This module contains the routines that manage information
 * about symbols: LITERALs, data symbols, and routines.  These
 * are usually just called 'names' in the LRM, but I use 'symbol'
 * to distinguish these names from names of other things.
 * COMPILETIME names are also handled in this module, even though
 * they are purely lexical entities.  LABELs are also managed
 * here.
 *
 * This module layers on top of the generic name table management
 * routines, which provide the underlying memory management and
 * name lookup mechanisms.  See nametable.c for further information.
 * Extension space is used for literals, routines, and data
 * symbols.  COMPILETIME names make use of the generic 'value'
 * storage already provided by the name table code.
 *
 * The parsing routines in declarations.c call on these routines
 * to add symbols to the symbol table, once the names and attributes
 * have been parsed.  A mechanism is provided to partially declare
 * a symbol, marking it "pending" until all attributes have been
 * parsed and the symbol information gets updated.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include "blissc/symbols.h"
#include "blissc/structures.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"
#include "blissc/expression.h"
#include "blissc/support/strings.h"
#include "blissc/support/utils.h"

/*
 * The following structures are the extensions to
 * the basic name type that hold the information specific
 * to the types of symbols tracked here.
 *
 * NB: The 'genspace' field MUST BE LAST, as the code generator
 * hook can extend these structures for its own purposes, and
 * those extensions are assumed to immediately follow that
 * field.
 */
struct sym_literal_s {
    literal_attr_t attr;
    void           *genspace;
};
typedef struct sym_literal_s sym_literal_t;

struct sym_label_s {
    expr_node_t     *block;
    void           *genspace;
};
typedef struct sym_label_s sym_label_t;

struct sym_data_s {
    name_t          *globalsym;
    data_attr_t      attr;
    initval_t       *ivlist_head, *ivlist_tail;
    void            *genspace;
};
typedef struct sym_data_s sym_data_t;

struct sym_routine_s {
    name_t          *globalsym;
    expr_node_t     *rtnexp;
    scopectx_t       argscope;
    routine_attr_t   attr;
    void            *genspace;
};
typedef struct sym_routine_s sym_routine_t;

struct sym_module_s {
    strdesc_t       *ident;
    strdesc_t       *mainrtn;
    expr_node_t     *modblock;
    void            *genspace;
};
typedef struct sym_module_s sym_module_t;

struct sym_psect_s {
    namereflist_t   symlist;
    unsigned int    attr;
    void           *genspace;
};
typedef struct sym_psect_s  sym_psect_t;

struct extenthdr_s {
    struct extenthdr_s *next;
};

/*
 * Context structure for this module.
 */
struct symctx_s {
    expr_ctx_t      expctx;
    namectx_t       namectx;
    logctx_t        logctx;
    strctx_t        strctx;
    machinedef_t   *mach;
    data_attr_t     data_defaults;
    routine_attr_t  routine_defaults;
    literal_attr_t  literal_defaults;
    void            *genctx;
    sym_genvec_t     genvec;
    unsigned int     gensize[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    struct extenthdr_s *extents;
    initval_t       *freeivs;
};

static const lextype_t symtype[6] = {
    LEXTYPE_NAME_LITERAL, LEXTYPE_NAME_DATA,
    LEXTYPE_NAME_ROUTINE, LEXTYPE_NAME_MODULE,
    LEXTYPE_NAME_LABEL, LEXTYPE_NAME_PSECT
};

#define IV_ALLOCOUNT 128

/*
 * uint_log2
 *
 * Table for logarithm of powers of 2, used below
 * for computing alignments.
 */
static int
uint_log2 (unsigned int n) {
    static int table[] = { -1, 0, 1, -1, 2, -1, -1, -1, 3 };
    if (n >= sizeof(table) || table[n] < 0) return 0;
    return table[n];
} /* uint_log2 */

/*
 * calc_width
 *
 * Compute a power-of-2 segment width from its allocation
 * size in units.
 */
static unsigned int
calc_width (unsigned int u, machinedef_t *mach) {
    unsigned int upval = machine_scalar_units(mach);
    if (u == 0 || u >= upval) return machine_scalar_bits(mach);
    while (uint_log2(u) < 0) u += 1;
    return machine_unit_bits(mach) * u;
} /* calc_width */

/*
 * data_free
 *
 * Destructor function for data symbols.
 * Invoked through name_free().
 */
static void
data_free (void *vctx, name_t *np, void *p)
{
    symctx_t symctx = vctx;
    namectx_t namectx = symctx->namectx;
    sym_data_t *d = p;

    if (symctx->genvec.genfree != 0) {
        (*symctx->genvec.genfree)(symctx->genctx, np, &d->genspace);
    }
    namereflist_free(namectx, &d->attr.fields);
    scope_end(d->attr.struscope);

} /* data_free */

/*
 * data_copy
 *
 * Copy function for data symbols.  Invoked
 * through name_copy().
 */
static int
data_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    symctx_t ctx = vctx;
    namectx_t namectx = ctx->namectx;
    sym_data_t *dsym = dp;
    sym_data_t *ssym = sp;
    nameref_t *ref;

    dsym->globalsym = ssym->globalsym;
    memcpy(&dsym->attr, &ssym->attr, sizeof(data_attr_t));
    dsym->attr.struscope = scope_copy(ssym->attr.struscope, 0);
    namereflist_init(&dsym->attr.fields);
    for (ref = namereflist_head(&ssym->attr.fields); ref != 0; ref = ref->tq_next) {
        name_t *np;
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *f = name_string(ref->np);
            np = name_search(dsym->attr.struscope, f->ptr, f->len, 0);
        }
        namereflist_instail(&dsym->attr.fields, nameref_alloc(namectx, np));
    }
    return 1;
}

/*
 * module_free
 *
 * Frees up a module cell.
 */
static void
module_free (void *vctx, name_t *np, void *p)
{
    symctx_t symctx = vctx;
    sym_module_t *m = p;

    if (m->ident != 0) string_free(symctx->strctx, m->ident);
    if (m->mainrtn != 0) string_free(symctx->strctx, m->mainrtn);
    if (symctx->genvec.genfree != 0) {
        (*symctx->genvec.genfree)(symctx->genctx, np, &m->genspace);
    }

} /* module_free */

/*
 * module_copy
 *
 * Copy-constructor for a module cell.
 */
static int
module_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    symctx_t symctx = vctx;
    sym_module_t *dm = dp;
    sym_module_t *sm = sp;

    if (sm->ident != 0) {
        dm->ident = string_copy(symctx->strctx, 0, sm->ident);
        if (dm->ident ==  0) return 0;
    }
    if (sm->mainrtn != 0) {
        dm->mainrtn = string_copy(symctx->strctx, 0, sm->mainrtn);
        if (dm->mainrtn == 0) return 0;
    }

    if (symctx->genvec.gencopy != 0) {
        (*symctx->genvec.gencopy)(symctx->genctx, dnp, &dm->genspace,
                                  snp, &sm->genspace);
    } else {
        memcpy(&dm->genspace, &sm->genspace,
               symctx->gensize[LEXTYPE_NAME_MODULE-LEXTYPE_NAME_MIN]);
    }

    return 1;

} /* module_copy */

/*
 * psect_free
 *
 * Destructor function for psect names.
 * Invoked through name_free().
 */
static void
psect_free (void *vctx, name_t *np, void *p)
{
    symctx_t symctx = vctx;
    sym_psect_t *d = p;

    namereflist_free(symctx->namectx, &d->symlist);
    if (symctx->genvec.genfree != 0) {
        (*symctx->genvec.genfree)(symctx->genctx, np, &d->genspace);
    }

} /* psect_free */

/*
 * psect_copy
 *
 * Copy-constructor function for psect names.
 * Invoked through name_copy().
 */
static int
psect_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    symctx_t symctx = vctx;
    sym_psect_t *d = dp, *s = sp;

    namereflist_copy(symctx->namectx, &d->symlist, &s->symlist);
    if (symctx->genvec.gencopy != 0) {
        return (*symctx->genvec.gencopy)(symctx->genctx,
                                         dnp, &d->genspace,
                                         snp, &s->genspace);
    }

    memcpy(&d->genspace, &s->genspace,
           symctx->gensize[LEXTYPE_NAME_PSECT-LEXTYPE_NAME_MIN]);
    return 1;

} /* psect_copy */

/*
 * passthru_init
 *
 * Generic constructor function.
 */
static int
passthru_init (void *vctx, name_t *np, void *p)
{
    symctx_t symctx = vctx;

    if (symctx->genvec.geninit == 0) {
        return 1;
    }

    switch (name_type(np)) {
        case LEXTYPE_NAME_LITERAL: {
            sym_literal_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        case LEXTYPE_NAME_DATA: {
            sym_data_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        case LEXTYPE_NAME_ROUTINE: {
            sym_routine_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        case LEXTYPE_NAME_LABEL: {
            sym_label_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        case LEXTYPE_NAME_MODULE: {
            sym_module_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        case LEXTYPE_NAME_PSECT: {
            sym_psect_t *sym = p;
            return (*symctx->genvec.geninit)(symctx->genctx, np, &sym->genspace);
        }
        default:
            break;
    }

    return 0;

} /* passthru_init */

/*
 * passthru_free
 *
 * Generic destructor function.
 */
static void
passthru_free (void *vctx, name_t *np, void *p)
{
    symctx_t symctx = vctx;

    if (symctx->genvec.genfree == 0) {
        return;
    }

    switch (name_type(np)) {
        case LEXTYPE_NAME_LITERAL: {
            sym_literal_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        case LEXTYPE_NAME_DATA: {
            sym_data_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        case LEXTYPE_NAME_ROUTINE: {
            sym_routine_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        case LEXTYPE_NAME_LABEL: {
            sym_label_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        case LEXTYPE_NAME_MODULE: {
            sym_module_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        case LEXTYPE_NAME_PSECT: {
            sym_psect_t *sym = p;
            (*symctx->genvec.genfree)(symctx->genctx, np, &sym->genspace);
            break;
        }
        default:
            break;
    }

} /* passthru_free */

/*
 * passthru_copy
 *
 * Generic copy-constructor function.
 */
static int
passthru_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    symctx_t symctx = vctx;


    switch (name_type(dnp)) {
        case LEXTYPE_NAME_LITERAL: {
            sym_literal_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_literal_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        case LEXTYPE_NAME_DATA: {
            sym_data_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_data_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        case LEXTYPE_NAME_ROUTINE: {
            sym_routine_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_routine_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        case LEXTYPE_NAME_LABEL: {
            sym_label_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_label_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        case LEXTYPE_NAME_MODULE: {
            sym_module_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_module_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        case LEXTYPE_NAME_PSECT: {
            sym_psect_t *dsym = dp, *ssym = sp;
            memcpy(dsym, ssym, sizeof(sym_psect_t));
            if (symctx->genvec.gencopy == 0) return 1;
            return (*symctx->genvec.gencopy)(symctx->genctx,
                                             dnp, &dsym->genspace,
                                             snp, &ssym->genspace);
        }
        default:
            break;
    }

    return 0;

} /* passthru_copy */

/*
 * bind_compiletime
 *
 * Binds a COMPILETIME name to its value.
 * This is a lexical binding function, invoked through
 * lexeme_bind.
 */
static int
bind_compiletime (lexctx_t lctx, void *vctx, quotelevel_t ql, quotemodifier_t qm,
                  lextype_t lt, condstate_t cs, lexeme_t *lex,
                  lexseq_t *result) {
    expr_ctx_t ctx = vctx;
    name_t *np = lexeme_ctx_get(lex);
    long val;

    if (cs == COND_CWA || cs == COND_AWC || ql == QL_MACROSKIP) {
        lexeme_free(lctx, lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    val = name_value_signed(np);
    string_free(expr_strctx(ctx), &lex->text);
    string_printf(expr_strctx(ctx), &lex->text, "%ld", val);
    lex->type = lex->boundtype = LEXTYPE_NUMERIC;

    return 0;

} /* bind_compiletime */

/*
 * bind_literal
 *
 * Binds a literal to its value.  Called by the
 * lexical binding function.
 */
static int
bind_literal (lexctx_t lctx, void *vctx, quotelevel_t ql, quotemodifier_t qm,
              lextype_t lt, condstate_t cs, lexeme_t *lex,
              lexseq_t *result) {
    expr_ctx_t ctx = vctx;
    name_t *np = lexeme_ctx_get(lex);
    sym_literal_t *lit = name_extraspace(np);
    long val;

    if (cs == COND_CWA || cs == COND_AWC || ql == QL_MACROSKIP) {
        lexeme_free(lctx, lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    if ((lit->attr.flags & SYM_M_NOVALUE)) {
        expr_signal(ctx, STC__LITNOVAL, lexeme_text(lex));
        val = 0;
    } else {
        val = getvalue(lit->attr.value, lit->attr.width,
                       (lit->attr.flags & SYM_M_SIGNEXT) != 0);
    }
    lexeme_type_set(lex, LEXTYPE_NUMERIC);
    string_free(expr_strctx(ctx), lexeme_text(lex));
    string_printf(expr_strctx(ctx), lexeme_text(lex), "%ld", val);
    return 0;

} /* bind_literal */

/*
 * bind_data
 *
 * Binds a data symbol to an expression node. Called
 * by the expression-binding routine.
 */
static expr_node_t *
bind_data (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    name_t *np = lexeme_ctx_get(lex);
    expr_node_t *exp;
    sym_data_t *sym;

    if (np == 0) {
        return 0;
    }
    sym = name_extraspace(np);

    exp = 0;
    if (sym->attr.struc != 0 &&
        parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
        exp = structure_reference(ctx, sym->attr.struc, 0, np, lex);
    } else {
        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, parser_curpos(pctx));
        expr_seg_name_set(exp, lexeme_ctx_get(lex));
        expr_seg_width_set(exp, sym->attr.width);
        expr_seg_signext_set(exp, (sym->attr.flags & SYM_M_SIGNEXT) != 0);
        expr_is_ltce_set(exp, sym->attr.dclass == DCLASS_STATIC);
        expr_has_value_set(exp, 1);
    }
    return exp;

} /* bind_data */

/*
 * bind_routine
 *
 * Binds a routine symbol to an expression node.
 */
static expr_node_t *
bind_routine (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    name_t *np = lexeme_ctx_get(lex);
    expr_node_t *exp;

    if (np == 0) {
        return 0;
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, parser_curpos(pctx));
    expr_seg_name_set(exp, np);
    expr_seg_width_set(exp, machine_addr_bits(mach));
    expr_seg_signext_set(exp, machine_addr_signed(mach));
    expr_is_ltce_set(exp, 1);
    expr_has_value_set(exp, 1);
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        exp = expr_parse_arglist(ctx, exp);
    }

    return exp;

} /* bind_routine */

/*
 * symbols_init
 *
 * Initialization for this module.  Sets up the context
 * structure, initializes the name table extension vectors,
 * and registers the lexical and expression binding functions.
 *
 */
symctx_t
symbols_init (expr_ctx_t ctx)
{

    namectx_t namectx = scope_namectx(parser_scope_get(expr_parse_ctx(ctx)));
    symctx_t symctx = malloc(sizeof(struct symctx_s));
    lexctx_t lctx = expr_lexmemctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);

    memset(symctx, 0, sizeof(struct symctx_s));
    symctx->logctx = expr_logctx(ctx);
    symctx->strctx = expr_strctx(ctx);
    symctx->expctx = ctx;
    symctx->mach = mach;
    symctx->namectx = namectx;
    symctx->data_defaults.dclass = DCLASS_STKORREG;
    symctx->data_defaults.sc = SYMSCOPE_LOCAL;
    symctx->data_defaults.units = machine_scalar_units(mach);
    symctx->data_defaults.width = machine_scalar_bits(mach);
    symctx->literal_defaults.width = machine_scalar_bits(mach);
    symctx->data_defaults.alignment = uint_log2(symctx->data_defaults.units);
    nametables_symctx_set(namectx, symctx);
    lextype_register(lctx, ctx, LEXTYPE_NAME_COMPILETIME, bind_compiletime);
    lextype_register(lctx, ctx, LEXTYPE_NAME_LITERAL, bind_literal);
    expr_dispatch_register(ctx, LEXTYPE_NAME_DATA, bind_data);
    expr_dispatch_register(ctx, LEXTYPE_NAME_ROUTINE, bind_routine);

    return symctx;

} /* symbols_init */

void
symbols_finish (symctx_t ctx)
{
    struct extenthdr_s *e, *enext;

    if (ctx == 0) {
        return;
    }

    for (e = ctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }

    free(ctx);

} /* symbols_finish */

/*
 * symbols_gen_register
 */
void
symbols_gen_register (symctx_t symctx, void *genctx, sym_genvec_t *vec)
{
    symctx->genctx = genctx;
    memcpy(&symctx->genvec, vec, sizeof(sym_genvec_t));
    
} /* symbols_gen_register */

/*
 * symbols_connect_hooks
 *
 * Register the nametype hooks with the nametable code.  Broken
 * out from symbols_init so the code generator can register its
 * extensions with this module first.
 */
void
symbols_connect_hooks (symctx_t symctx)
{
    namectx_t namectx = symctx->namectx;
    nametype_vectors_t sv;
    int i;

    static nametype_vectors_t symvec[6] = {
        { sizeof(sym_literal_t), passthru_init, passthru_free, passthru_copy },
        { sizeof(sym_data_t), passthru_init, data_free, data_copy },
        { sizeof(sym_routine_t), passthru_init, passthru_free, passthru_copy },
        { sizeof(sym_module_t), passthru_init, module_free, module_copy },
        { sizeof(sym_label_t), passthru_init, passthru_free, passthru_copy },
        { sizeof(sym_psect_t), passthru_init, psect_free, psect_copy }
    };
    for (i = 0; i < sizeof(symvec)/sizeof(symvec[0]); i++) {
        memcpy(&sv, &symvec[i], sizeof(sv));
        if (symctx->genvec.sizefn != 0) {
            unsigned int extra = (*symctx->genvec.sizefn)(symctx->genctx, symtype[i]);
            // Account for the field we already allocate for this (genspace, a void *)
            if (extra != 0) extra = (extra > sizeof(void *) ? extra-sizeof(void *) : 0);
            symctx->gensize[symtype[i]-LEXTYPE_NAME_MIN] = extra;
            sv.typesize += symctx->gensize[symtype[i]-LEXTYPE_NAME_MIN];
        }
        nametype_dataop_register(namectx, symtype[i], &sv, symctx);
    }

} /* symbols_connect_hooks */

/*
 * datasym_search
 *
 * Convenience routine for looking up a data symbol.
 */
name_t *
datasym_search (scopectx_t scope, strdesc_t *namedsc, data_attr_t *attrp)
{
    name_t *np;
    sym_data_t *sym;

    np = name_search_typed(scope, namedsc->ptr, namedsc->len,
                             LEXTYPE_NAME_DATA, &sym);
    if (np != 0 && attrp != 0) {
        memcpy(attrp, &sym->attr, sizeof(sym->attr));
    }
    return np;

} /* datasym_search */

/*
 * compare_data_attrs
 *
 * Internal function that compares to sets of data
 * attributes to see if they are equivalent.  This
 * is used when checking FORWARD declarations against
 * later instantiations of the same symbol, as well
 * as EXTERNAL/GLOBAL declarations.
 */
static int
compare_data_attrs (data_attr_t *a, data_attr_t *b) {

    nameref_t *aref, *bref;

    if (a->units != b->units ||
        a->width != b->width ||
        a->struc != b->struc ||
        a->owner != b->owner ||
        a->flags != b->flags ||
        namereflist_length(&a->fields) != namereflist_length(&b->fields)) {
        return 0;
    }

    // Check that the field names are the same.  Since the names
    // aren't kept in a particular order, we just do the O(n**2)
    // walk through the lists.
    // XXX A more efficient implementation should be considered. XXX
    for (aref = namereflist_head(&a->fields); aref != 0; aref = aref->tq_next) {
        for (bref = namereflist_head(&b->fields);
             bref->np != aref->np && bref != 0; bref = bref->tq_next);
        if (bref == 0) return 0;
    }

    return 1;

} /* compare_data_attrs */

/*
 * datasym_declare
 *
 * Declare a data symbol (OWN, LOCAL, STACKLOCAL, GLOBAL,
 * EXTERNAL, FORWARD).
 */
name_t *
datasym_declare (scopectx_t scope, strdesc_t *dsc, data_attr_t *attrp, textpos_t pos)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    name_t *np, *gnp = 0;
    sym_data_t *sym, *gsym;
    data_attr_t attr;
    namedef_t ndef;

    if (attrp == 0) {
        memcpy(&attr, &symctx->data_defaults, sizeof(data_attr_t));
        attrp = &attr;
    } else if (attrp->units != 0) {
        if (attrp->alignment == 0) {
            if (attrp->units <= machine_scalar_units(symctx->mach)) {
                attrp->alignment = uint_log2(attrp->units);
            } else {
                // XXX should this be another machine setting?
                attrp->alignment = uint_log2(machine_scalar_units(symctx->mach));
            }
        }
        if (attrp->width == 0) {
            attrp->width = calc_width(attrp->units, symctx->mach);
        }
    }

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_DATA;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    if (attrp->flags & SYM_M_FORWARD) {
        ndef.flags |= NAME_M_FORWARD;
    }

    if (attrp->sc == SYMSCOPE_EXTERNAL || attrp->sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(gscope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_DATA, &gsym);
        if (gnp != 0 && !(attrp->flags & SYM_M_PENDING)) {
            if (!compare_data_attrs(&gsym->attr, attrp)) {
                return 0; // attribute mismatch
            }
        }
        if (gnp == 0) {
            gnp = name_declare(gscope, &ndef, pos, 0, 0, &gsym);
            memcpy(&gsym->attr, attrp, sizeof(data_attr_t));
        }
    }
    np = name_declare(scope, &ndef, pos, 0, 0, &sym);
    if (np != 0) {
        if (sym->attr.flags & SYM_M_FORWARD) {
            if (attrp->flags & SYM_M_FORWARD) {
                log_signal(symctx->logctx, pos, STC__REDECLARE, dsc->ptr, dsc->len);
            } else if (!(attrp->flags & SYM_M_PENDING) &&
                       !compare_data_attrs(&sym->attr, attrp)) {
                log_signal(symctx->logctx, pos, STC__ATTRNCMPT, dsc);
            }
        }
        memcpy(&sym->attr, attrp, sizeof(data_attr_t));
        if (attrp->sc == SYMSCOPE_EXTERNAL || attrp->sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
        if (!(attrp->flags & SYM_M_PENDING) && symctx->genvec.genfn != 0) {
            (*symctx->genvec.genfn)(symctx->genctx, np, &sym->genspace);
        }
    }

    return np;

} /* datasym_declare */

/*
 * datasym_attr_update
 *
 * Updates the attributes of a symbol created by
 * datasym_declare().  Typically used to commit the
 * fully-known attribute set to the symbol after an
 * initial "pending" creation.
 */
int
datasym_attr_update (name_t *np, data_attr_t *attrp)
{

    symctx_t symctx = nametables_symctx_get(scope_namectx(name_scope(np)));
    sym_data_t *sym = name_extraspace(np);
    sym_data_t *gsym = 0;

    if (sym->globalsym != 0) {
        gsym = name_extraspace(sym->globalsym);
        if (!(gsym->attr.flags & SYM_M_PENDING) &&
            !compare_data_attrs(&gsym->attr, attrp)) {
            log_signal(symctx->logctx, name_defpos(np), STC__ATTRNCMPT, name_string(np));
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(data_attr_t));
    if (attrp->width == 0) {
        sym->attr.width = calc_width(attrp->units, symctx->mach);
    }
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(data_attr_t));
    }
    if (!(attrp->flags & SYM_M_PENDING) && symctx->genvec.genfn != 0) {
        (*symctx->genvec.genfn)(symctx->genctx, np, &sym->genspace);
    }
    return 1;

} /* datasym_attr_update */

/*
 * Getter/setter routines for data symbols
 *
 */
data_attr_t *datasym_attr (name_t *np) { sym_data_t *sym = name_extraspace(np);
    return &sym->attr; }
/*
 * compiletime_declare
 *
 * Declares a COMPILETIME name.  No two-phase commit for
 * these, as they have no attributes, just a value (which
 * is stored directly in the default 'value' space in
 * the name cell).
 */
name_t *
compiletime_declare (scopectx_t scope, strdesc_t *dsc, long val, textpos_t pos)
{
    namedef_t ndef;
    name_t *np;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_COMPILETIME;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    np = name_declare(scope, &ndef, pos, 0, 0, 0);
    if (np != 0) name_value_signed_set(np, val);
    return np;

} /* compiletime_declare */

/*
 * Set/get COMPILETIME values
 */
void compiletime_assign (name_t *np, long val) { name_value_signed_set(np, val); }
long compiletime_value (name_t *np) { return name_value_signed(np); }

/*
 * label_declare
 *
 * Add a label to the name table.
 */
name_t *
label_declare (scopectx_t scope, strdesc_t *dsc, textpos_t pos)
{
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_LABEL;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    return name_declare(scope, &ndef, pos, 0, 0, 0);

} /* label_declare */

/*
 * label_block_set
 *
 * This is the point where a label gets instantiated, so
 * we call the code generator function here, rather than
 * in label_declare().  So this is more than your typical
 * setter function.
 */
void
label_block_set (name_t *np, expr_node_t *exp) {
    sym_label_t *lbl = name_extraspace(np);
    symctx_t symctx = nametables_symctx_get(scope_namectx(name_scope(np)));

    lbl->block = exp;
    if (symctx->genvec.genfn != 0) {
        (*symctx->genvec.genfn)(symctx->genctx, np, &lbl->genspace);
    }
}
expr_node_t *label_block (name_t *np) { sym_label_t *lbl = name_extraspace(np);
    return lbl->block;
}

/*
 * litsym_search
 *
 * Courtesy routine for looking up a LITERAL by name.  If
 * found, the value can also be returned.
 */
name_t *
litsym_search (scopectx_t scope, strdesc_t *dsc, unsigned long *valp)
{
    name_t *np;
    sym_literal_t *sym;
    np = name_search_typed(scope, dsc->ptr, dsc->len, LEXTYPE_NAME_LITERAL, &sym);
    if (np != 0 && valp != 0) {
        *valp = getvalue(sym->attr.value, sym->attr.width,
                         (sym->attr.flags & SYM_M_SIGNEXT) != 0);
    }
    return np;

} /* litsym_search */

/*
 * litsym_declare
 *
 * Declares a LITERAL.
 */
name_t *
litsym_declare (scopectx_t scope, strdesc_t *dsc, literal_attr_t *attrp, textpos_t pos)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    literal_attr_t attr;
    sym_literal_t *sym;
    name_t *np;
    namedef_t ndef;

    if (attrp == 0) {
        memcpy(&attr, &symctx->literal_defaults, sizeof(literal_attr_t));
        attrp = &attr;
    }

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_LITERAL;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    ndef.flags |= (attrp->flags & SYM_M_RESERVED) ? NAME_M_RESERVED : 0;

    if (attrp->sc == SYMSCOPE_EXTERNAL || attrp->sc == SYMSCOPE_GLOBAL) {
        name_t *gnp = 0;
        sym_literal_t *gsym;
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(gscope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_LITERAL, &gsym);
        if (gnp != 0) {
            if (gsym->attr.width != attrp->width ||
                (gsym->attr.flags & SYM_M_SIGNEXT)
                != (attrp->flags & SYM_M_SIGNEXT)) {
                log_signal(symctx->logctx, pos, STC__ATTRNCMPT, dsc);
                return 0;
            }
        }
        if (gnp == 0) {
            gnp = name_declare(gscope, &ndef, pos, 0, 0, &gsym);
            if (gnp == 0) {
                log_signal(symctx->logctx, pos, STC__INTCMPERR, "litsym_declare");
            }
            if (gsym != 0) memcpy(&gsym->attr, attrp, sizeof(literal_attr_t));
        }
    }
    np = name_declare(scope, &ndef, pos, 0, 0, &sym);
    if (np != 0 && sym != 0) {
        memcpy(&sym->attr, attrp, sizeof(literal_attr_t));
        if (symctx->genvec.genfn != 0) {
            (*symctx->genvec.genfn)(symctx->genctx, np, &sym->genspace);
        }
    }
    return np;

} /* litsym_declare */

literal_attr_t *litsym_attr (name_t *np) { sym_literal_t *sym = name_extraspace(np);
    return &sym->attr; }

/*
 * litsym_special
 *
 * For internal use by the compiler in appropriate circumstances
 * only, this routine is for defining LITERALs in a local name
 * scope only, and overrides the normal checks for user-defined
 * names.  Note also that these 'specials' get no code generation.
 *
 */
name_t *
litsym_special (scopectx_t scope, strdesc_t *dsc, unsigned long value)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    literal_attr_t attr;
    namedef_t ndef;
    name_t *np;
    sym_literal_t *sym;

    memcpy(&attr, &symctx->literal_defaults, sizeof(literal_attr_t));
    attr.value = getvalue(value, attr.width, (attr.flags & SYM_M_SIGNEXT) != 0);

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_LITERAL;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;

    np = name_declare_nocheck(scope, &ndef, 0, 0, 0, &sym);
    if (np != 0) memcpy(&sym->attr, &attr, sizeof(literal_attr_t));
    return np;

} /* litsym_special */

/*
 * rtnsym_search
 *
 * Courtesy routine for looking up a routine name.
 */
name_t *
rtnsym_search (scopectx_t scope, strdesc_t *namedsc)
{
    return name_search_typed(scope, namedsc->ptr, namedsc->len,
                           LEXTYPE_NAME_ROUTINE, 0);
} /* rtnsym_search */

/*
 * compare_routine_attrs
 *
 * Internal function for checking compatibility of
 * routine attributes (for FORWARD/normal, EXTERNAL/GLOBAL
 * routines, for example).
 */
static int
compare_routine_attrs (routine_attr_t *a, routine_attr_t *b) {

    if ((a->flags & SYM_M_NOVALUE) != (b->flags & SYM_M_NOVALUE)) {
        return 0;
    }

    return 1;

} /* compare_routine_attrs */

/*
 * rtnsym_declare
 *
 * Declares a routine symbol.
 */
name_t *
rtnsym_declare (scopectx_t scope, strdesc_t *dsc, routine_attr_t *attrp, textpos_t pos)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    name_t *np, *gnp = 0;
    sym_routine_t *sym, *gsym;
    routine_attr_t attr;
    namedef_t ndef;

    if (attrp == 0) {
        memcpy(&attr, &symctx->data_defaults, sizeof(routine_attr_t));
        attrp = &attr;
    }

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_ROUTINE;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    if (attrp->flags & SYM_M_FORWARD) {
        ndef.flags |= NAME_M_FORWARD;
    }

    if (attrp->sc == SYMSCOPE_EXTERNAL || attrp->sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(gscope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_ROUTINE, &gsym);
        if (gnp != 0 && !(attrp->flags & SYM_M_PENDING)) {
            if (!compare_routine_attrs(&gsym->attr, attrp)) {
                log_signal(symctx->logctx, pos, STC__ATTRNCMPT, dsc);
                return 0; // attribute mismatch
            }
        }
        if (gnp == 0) {
            gnp = name_declare(gscope, &ndef, pos, 0, 0, &gsym);
            if (gsym != 0) memcpy(&gsym->attr, attrp, sizeof(routine_attr_t));
        }
    }
    np = name_declare(scope, &ndef, pos, 0, 0, &sym);
    if (np != 0) {
        memcpy(&sym->attr, attrp, sizeof(routine_attr_t));
        if (attrp->sc == SYMSCOPE_EXTERNAL || attrp->sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
        if (!(sym->attr.flags & SYM_M_PENDING) && symctx->genvec.genfn != 0) {
            (*symctx->genvec.genfn)(symctx->genctx, np, &sym->genspace);
        }
    }

    return np;

} /* rtnsym_declare */

/*
 * rtnsym_attr_update
 *
 * Update a routine symbol's attributes.  Again, this
 * is intended to be used during two-phase declarations;
 * a PENDING entry gets created with rtnsym_declare(), with
 * this routine being called once all attributes are parsed,
 * to finalize the entry.
 */
int
rtnsym_attr_update (name_t *np, routine_attr_t *attrp)
{
    symctx_t symctx = nametables_symctx_get(scope_namectx(name_scope(np)));
    sym_routine_t *sym = name_extraspace(np);
    sym_routine_t *gsym = 0;

    if (sym->globalsym != 0) {
        gsym = name_extraspace(sym->globalsym);
        if (!(gsym->attr.flags & SYM_M_PENDING) &&
            !compare_routine_attrs(&gsym->attr, attrp)) {
            log_signal(symctx->logctx, name_defpos(np), STC__ATTRNCMPT, name_string(np));
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(routine_attr_t));
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(routine_attr_t));
    }
    if (!(sym->attr.flags & SYM_M_PENDING) && symctx->genvec.genfn != 0) {
        (*symctx->genvec.genfn)(symctx->genctx, np, &sym->genspace);
    }
    return 1;

} /* rtnsym_attr_update */

/*
 * Getter/setter functions for routine symbols
 */
routine_attr_t * rtnsym_attr (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return &sym->attr; }
expr_node_t *rtnsym_expr (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return sym->rtnexp; }
void rtnsym_expr_set (name_t *np, expr_node_t *exp) { sym_routine_t *sym = name_extraspace(np);
    sym->rtnexp = exp; if (sym->globalsym) rtnsym_expr_set(sym->globalsym, exp); }
/*
 * modsym_declare
 *
 * Declares a module symbol.
 */
name_t *
modsym_declare (scopectx_t scope, strdesc_t *dsc, textpos_t pos)
{
    namedef_t ndef;
    name_t *np;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_MODULE;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    ndef.flags = 0;

    np = name_declare(scope, &ndef, pos, 0, 0, 0);

    return np;
    
} /* modsym_declare */

/*
 * Getters/setters for module symbols
 */
strdesc_t *modsym_ident (name_t *np) {
    sym_module_t *m = name_extraspace(np);
    return name_type(np) == LEXTYPE_NAME_MODULE ? m->ident : 0;
}
void modsym_ident_set (name_t *np, strdesc_t *str) {
    if (name_type(np) == LEXTYPE_NAME_MODULE) {
        sym_module_t *m = name_extraspace(np);
        m->ident = str;
    }
}
strdesc_t *modsym_main (name_t *np) {
    sym_module_t *m = name_extraspace(np);
    return name_type(np) == LEXTYPE_NAME_MODULE ? m->mainrtn : 0;
}
void modsym_main_set (name_t *np, strdesc_t *str) {
    if (name_type(np) == LEXTYPE_NAME_MODULE) {
        sym_module_t *m = name_extraspace(np);
        m->mainrtn = str;
    }
}
expr_node_t *modsym_block (name_t *np) {
    sym_module_t *m = name_extraspace(np);
    return name_type(np) == LEXTYPE_NAME_MODULE ? m->modblock : 0;
}
void modsym_block_set (name_t *np, expr_node_t *blk) {
    if (name_type(np) == LEXTYPE_NAME_MODULE) {
        sym_module_t *m = name_extraspace(np);
        m->modblock = blk;
    }
}



/*
 * psect_declare
 *
 * Declares a PSECT name, registering it with
 * the storage manager.
 */
name_t *
psect_declare (scopectx_t scope, strdesc_t *dsc,
               unsigned int psflags, textpos_t pos)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    namedef_t ndef;
    sym_psect_t *psect;
    name_t *np;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_PSECT;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    np = name_declare(scope, &ndef, pos, 0, 0, &psect);
    if (np != 0) {
        psect->attr = psflags;
    }
    if (symctx->genvec.genfn != 0) {
        (*symctx->genvec.genfn)(symctx->genctx, np, &psect->genspace);
    }
    return np;

} /* psect_declare */

/*
 * psect_search
 *
 * Looks up a PSECT by name.
 */
name_t *
psect_search (scopectx_t scope, strdesc_t *dsc)
{
    return name_search_typed(scope, dsc->ptr, dsc->len, LEXTYPE_NAME_PSECT, 0);

} /* psect_search */

/*
 * Psect getter/setters
 */
unsigned int psect_attr (name_t *np) { sym_psect_t *ps = name_extraspace(np);
    return ps->attr; }

/*
 * sym_undeclare
 *
 * This function should be called to "undeclare" any user-declarable
 * symbol.  It does some validation then calls on name_undeclare()
 * to do the real work.
 */
int
sym_undeclare (scopectx_t scope, strdesc_t *dsc, textpos_t pos)
{
    lextype_t type;
    name_t *np = name_search(scope, dsc->ptr, dsc->len, &type);

    if (np == 0 || (type < LEXTYPE_NAME_MIN || type > LEXTYPE_NAME_MAX)) {
        namectx_t namectx = scope_namectx(scope);
        symctx_t symctx = nametables_symctx_get(namectx);
        log_signal(symctx->logctx, pos, STC__UNDECUND, dsc);
        return 0;
    }
    return name_undeclare(scope, np, pos);

} /* sym_undeclare */


/*
 * get_sym_base
 *
 * Internal routine, used to determine whether a symbol
 * is a link-time constant.  It is if it has static
 * storage allocated (i.e., storage in a psect), or if
 * it is an external symbol.  Note that this routine
 * handles both data and routine symbols; the logic is
 * the same, but the data structures are different.
 */
name_t *
get_sym_base (name_t *np)
{
    sym_data_t *d = name_extraspace(np);
    sym_routine_t *r = name_extraspace(np);

    if (name_type(np) == LEXTYPE_NAME_DATA) {
        return (d->globalsym == 0 ? (d->attr.dclass == DCLASS_STATIC ? d->attr.owner : 0)
                : d->globalsym);
    } else if (name_type(np) == LEXTYPE_NAME_ROUTINE) {
        if (r->globalsym != 0) {
            return r->globalsym;
        }
        if (r->attr.flags & SYM_M_BIND) {
            if (name_type(r->attr.owner) == LEXTYPE_NAME_PSECT) {
                return r->attr.owner;
            } else {
                return 0;
            }
        }
    }

    return 0;

} /* get_sym_base */

/*
 * sym_addrs_comparable
 *
 * Returns 1 if the two symbols are link-time
 * constants *and* are either located in the
 * same psect, or both reference the same
 * external symbol.
 */
int
sym_addrs_comparable (name_t *np_a, name_t *np_b)
{
    name_t *a_base, *b_base;

    a_base = get_sym_base(np_a);
    b_base = get_sym_base(np_b);

    return (a_base != 0 && (a_base == b_base));

} /* sym_addrs_comparable */

/*
 * sym_check_dangling_forwards
 *
 * Utility routine for walking the local name scope
 * for FORWARD declarations that have not been later
 * declared as actual data/routine names.
 */
void
sym_check_dangling_forwards (scopectx_t scope, textpos_t pos)
{
    void *walkctx = 0;
    name_t *np;
    int is_dangling;

    for (np = scope_nextname(scope, &walkctx); np != 0;
         np = scope_nextname(scope, &walkctx)) {
        is_dangling = 0;
        if (name_type(np) == LEXTYPE_NAME_DATA) {
            sym_data_t *dsym = name_extraspace(np);
            is_dangling = (dsym->attr.flags & SYM_M_FORWARD) != 0;
        } else if (name_type(np) == LEXTYPE_NAME_ROUTINE) {
            sym_routine_t *rsym = name_extraspace(np);
            is_dangling = (rsym->attr.flags & SYM_M_FORWARD) != 0;
        }
        if (is_dangling) {
            symctx_t symctx = nametables_symctx_get(scope_namectx(scope));
            log_signal(symctx->logctx, pos, STC__FWDNOTDCL, name_string(np));
        }
    }

} /* sym_check_dangling_forwards */

/*
 * sym_genspace
 *
 * Returns a pointer to the code generator's reserved space, for
 * those name types that have such a thing.
 */
void *
sym_genspace (name_t *np)
{
    void *p = name_extraspace(np);

    switch (name_type(np)) {
        case LEXTYPE_NAME_LITERAL: {
            sym_literal_t *sym = p;
            return &sym->genspace;
        }
        case LEXTYPE_NAME_DATA: {
            sym_data_t *sym = p;
            return &sym->genspace;
        }
        case LEXTYPE_NAME_ROUTINE: {
            sym_routine_t *sym = p;
            return &sym->genspace;
        }
        case LEXTYPE_NAME_LABEL: {
            sym_label_t *sym = p;
            return &sym->genspace;
        }
        case LEXTYPE_NAME_MODULE: {
            sym_module_t *sym = p;
            return &sym->genspace;
        }
        case LEXTYPE_NAME_PSECT: {
            sym_psect_t *sym = p;
            return &sym->genspace;
        }
        default:
            break;
    }
    
    return 0;
    
} /* sym_genspace */

/*
 * initval_alloc
 *
 * Allocate an initval structure.
 */
initval_t *
initval_alloc (symctx_t ctx)
{
    initval_t *iv;

    if (ctx->freeivs == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s) + sizeof(initval_t)*IV_ALLOCOUNT);
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freeivs = (initval_t *)(extent + 1);
        for (i = 0, iv = ctx->freeivs; i < IV_ALLOCOUNT-1; i++, iv++) {
            iv->next = iv + 1;
        }
        iv->next = 0;
    }
    iv = ctx->freeivs;
    ctx->freeivs = iv->next;
    memset(iv, 0, sizeof(initval_t));
    return iv;

} /* initval_alloc */
/*
 * initval_freelist
 *
 * Frees a linked list of initval_t structures and
 * their contents.  Reentrant and recursive, as
 * an initval can point to another list of initvals.
 */
void
initval_freelist (symctx_t ctx, initval_t *iv)
{
    initval_t *nextiv;

    while (iv != 0) {
        nextiv = iv->next;
        switch (iv->type) {
            case IVTYPE_STRING:
                string_free(ctx->strctx, iv->data.string);
                break;
            case IVTYPE_EXPR_EXP:
                break; // XXX possible memory leak here?
            case IVTYPE_LIST:
                initval_freelist(ctx, iv->data.listptr);
            case IVTYPE_SCALAR:
                break;
        }
        iv->next = ctx->freeivs;
        ctx->freeivs = iv;
        iv = nextiv;
    }

} /* initval_freelist */

/*
 * initval_scalar_add
 *
 * Adds a scalar initialization value to a list.
 */
initval_t *
initval_scalar_add (symctx_t ctx, initval_t *listhead, unsigned int reps,
                    long val, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = reps;
    iv->data.scalar.expr = 0;
    iv->data.scalar.value = val;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = signext;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_scalar_add */

/*
 * preset_scalar_add
 *
 * Adds a scalar expression to an initval list.
 */
initval_t *
preset_scalar_add (symctx_t ctx, initval_t *listhead, void *pexp, long val)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = 0;
    iv->preset_expr = pexp;
    iv->data.scalar.expr = 0;
    iv->data.scalar.value = val;
    iv->data.scalar.width = 0;
    iv->data.scalar.signext = 0;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* preset_scalar_add */

/*
 * initval_expr_add
 *
 * Adds an expression initializer to an initval list.
 */
initval_t *
initval_expr_add (symctx_t ctx, initval_t *listhead, unsigned int reps,
                  void *exp, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_EXPR_EXP;
    iv->repcount = reps;
    iv->data.scalar.expr  = exp;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = signext;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_expr_add */

/*
 * preset_expr_add
 *
 * Adds an expression to an initval list used for PRESET.
 */
initval_t *
preset_expr_add (symctx_t ctx, initval_t *listhead, void *pexp, void *exp)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_EXPR_EXP;
    iv->repcount = 0;
    iv->preset_expr = pexp;
    iv->data.scalar.expr  = exp;
    iv->data.scalar.width = 0;
    iv->data.scalar.signext = 0;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* preset_expr_add */

/*
 * initval_scalar_prepend
 *
 * Prepends a scalar value to an initval list.   Used
 * for PLIT construction (for the fullword count).
 */
initval_t *
initval_scalar_prepend (symctx_t ctx, initval_t *listhead, unsigned int reps,
                        long val, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = reps;
    iv->data.scalar.value = val;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = signext;
    iv->next = listhead;
    if (listhead == 0) {
        iv->lastptr = iv;
    }
    return iv;

} /* initval_scalar_prepend */

/*
 * initval_string_add
 *
 * Adds a literal string initializer.
 */
initval_t *
initval_string_add (symctx_t ctx, initval_t *listhead, unsigned int reps,
                    strdesc_t *str)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_STRING;
    iv->repcount = reps;
    iv->data.string = string_copy(ctx->strctx, 0, str);
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_string_add */

/*
 * initval_ivlist_add
 *
 * Adds an initval list as a sublist to the current list.
 */
initval_t *
initval_ivlist_add (symctx_t ctx, initval_t *listhead, unsigned int reps,
                    initval_t *sublist)
{
    initval_t *iv, *nextiv;


    // Simplify if there's only one item in the sublist.
    if (sublist->next == 0) {
        sublist->repcount = reps * sublist->repcount;
        reps = 1;
    }

    // If there's no repeat count, just append the sublist
    if (reps == 1) {
        if (listhead == 0) {
            return sublist;
        }
        for (iv = sublist; iv != 0; iv = nextiv) {
            nextiv = iv->next;
            iv->next = 0;
            listhead->lastptr->next = iv;
            listhead->lastptr = iv;
        }
        return listhead;
    }

    iv = initval_alloc(ctx);
    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_LIST;
    iv->repcount = reps;
    iv->data.listptr = sublist;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_ivlist_add */

/*
 * initval_size
 *
 * Computes the size of an initval list, taking into
 * account repeat counts, sublists, etc.
 */
unsigned long
initval_size (symctx_t ctx, initval_t *ivlist)
{
    initval_t *iv;
    unsigned long totsize = 0;

    if (ivlist->preset_expr == 0) {
        for (iv = ivlist; iv != 0; iv = iv->next) {
            switch (iv->type) {
                case IVTYPE_SCALAR:
                case IVTYPE_EXPR_EXP:
                    totsize += iv->repcount * iv->data.scalar.width;
                    break;
                case IVTYPE_STRING:
                    totsize += ((iv->repcount * iv->data.string->len +
                                 machine_unit_maxbytes(ctx->mach))-1) /
                    machine_unit_maxbytes(ctx->mach);
                    break;
                case IVTYPE_LIST:
                    totsize += iv->repcount * initval_size(ctx, iv->data.listptr);
                    break;
            }
        }
    } else {
        machinedef_t *mach = ctx->mach;
        unsigned long bpunit = machine_unit_bits(mach);

        // For PRESETs, go through each of the preset offset expressions
        // and locate the furthest one (taking into account the size of
        // the field, too).
        //
        // This should work because the address arithmetic expressions that
        // come out of a structure reference involve CTCE offsets, so the
        // constant folding in the expression parser will eliminate the
        // arithmetic expressions for us.
        for (iv = ivlist; iv != 0; iv = iv->next) {
            expr_node_t *exp = iv->preset_expr;
            unsigned long thissize;
            if (expr_type(exp) == EXPTYPE_PRIM_STRUREF) {
                exp = expr_struref_accexpr(exp);
            }
            if (expr_type(exp) == EXPTYPE_PRIM_SEG) {
                thissize = expr_seg_offset(exp) + (expr_seg_width(exp)/bpunit);
            } else if (expr_type(exp) == EXPTYPE_PRIM_FLDREF) {
                expr_node_t *pos = expr_fldref_pos(exp);
                expr_node_t *siz = expr_fldref_size(exp);
                thissize = expr_seg_offset(expr_fldref_addr(exp));
                if (expr_type(pos) != EXPTYPE_PRIM_LIT ||
                    expr_type(siz) != EXPTYPE_PRIM_LIT) {
                    expr_signal(ctx->expctx, STC__PROFNCTCE);
                } else {
                    // round up to the next unit
                    thissize += ((expr_litval(pos) + expr_litval(siz) + bpunit-1) /
                                 bpunit);
                }
            } else {
                expr_signal(ctx->expctx, STC__PRBADEXPR);
                continue;
            }
            if (thissize > totsize) {
                totsize = thissize;
            }
        }
    }
    return totsize;
    
} /* initval_size */
