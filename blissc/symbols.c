//
//  symbols.c
//  blissc
//
//  Created by Matthew Madison on 12/7/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "symbols.h"
#include "structures.h"
#include "nametable.h"
#include "lexeme.h"
#include "expression.h"
#include "storage.h"
#include "strings.h"
#include "utils.h"

#define ALLOC_QTY 128
typedef enum {
    SYM_LITERAL,
    SYM_DATA,
    SYM_ROUTINE
} symlist_t;
#define SYMLIST_COUNT 3

struct sym_literal_s {
    literal_attr_t attr;
};
typedef struct sym_literal_s sym_literal_t;

struct sym_data_s {
    name_t          *globalsym;
    seg_t           *seg;
    data_attr_t      attr;
};
typedef struct sym_data_s sym_data_t;

struct sym_routine_s {
    name_t          *globalsym;
    expr_node_t     *rtnexp;
    frame_t         *stack;
    seg_t           *seg;
    scopectx_t       argscope;
    routine_attr_t   attr;
};
typedef struct sym_routine_s sym_routine_t;

struct symctx_s {
    stgctx_t        stg;
    data_attr_t     data_defaults;
    routine_attr_t  routine_defaults;
    literal_attr_t  literal_defaults;
};
typedef struct symctx_s *symctx_t;

static const lextype_t symtype[3] = {
    LEXTYPE_NAME_LITERAL, LEXTYPE_NAME_DATA, LEXTYPE_NAME_ROUTINE
};

static void
data_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    sym_data_t *d = p;

    namereflist_free(namectx, &d->attr.fields);
    scope_end(d->attr.struscope);

} /* data_free */

static int
data_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    sym_data_t *dsym = dp;
    sym_data_t *ssym = sp;
    nameref_t *ref;

    dsym->globalsym = ssym->globalsym;
    dsym->seg = ssym->seg;
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

#if 0
static void
routine_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    sym_routine_t *r = p;

    expr_node_free(ctx, r->rtnexp);
    scope_end(r->argscope);

} /* routine_free */
#endif

/*
 * bind_compiletime
 *
 * Binds a COMPILETIME name to its value.
 * This is a lexical binding function, invoked through
 * lexeme_bind.
 */
static int
bind_compiletime (void *ctx, quotelevel_t ql, quotemodifier_t qm,
                  lextype_t lt, condstate_t cs, lexeme_t *lex,
                  lexseq_t *result) {
    name_t *np = lexeme_ctx_get(lex);
    long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    val = name_value_signed(np);
    string_free(&lex->text);
    string_printf(&lex->text, "%ld", val);
    lex->type = lex->boundtype = LEXTYPE_NUMERIC;
    lexeme_val_setsigned(lex, val);

    return 0;

} /* bind_compiletime */

/*
 * bind_literal
 *
 * Binds a literal to its value.
 */
static int
bind_literal (void *ctx, quotelevel_t ql, quotemodifier_t qm,
              lextype_t lt, condstate_t cs, lexeme_t *lex,
              lexseq_t *result) {
    name_t *np = lexeme_ctx_get(lex);
    sym_literal_t *lit = name_extraspace(np);
    long val;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(lex);
        return 1;
    }

    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }
    if ((lit->attr.flags & SYM_M_NOVALUE)) {
        /* XXX error condition */
        val = 0;
    } else {
        val = getvalue(lit->attr.value, lit->attr.width,
                       (lit->attr.flags & SYM_M_SIGNEXT) != 0);
    }
    lexeme_type_set(lex, LEXTYPE_NUMERIC);
    string_free(lexeme_text(lex));
    string_printf(lexeme_text(lex), "%ld", val);
    lexeme_val_setsigned(lex, val);
    return 0;

} /* bind_literal */

/*
 * bind_data
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
        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG,
                              lexeme_textpos_get(lex));
        expr_seg_name_set(exp, lexeme_ctx_get(lex));
        expr_seg_base_set(exp, sym->seg);
        expr_seg_units_set(exp, sym->attr.units);
        expr_seg_signext_set(exp, (sym->attr.flags & SYM_M_SIGNEXT) != 0);
        expr_is_ltce_set(exp, seg_addr_is_ltce(sym->seg));
    }
    return exp;

} /* bind_data */

/*
 * bind_routine
 */
static expr_node_t *
bind_routine (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);
    name_t *np = lexeme_ctx_get(lex);
    expr_node_t *exp;
    sym_routine_t *sym;

    if (np == 0) {
        return 0;
    }
    sym = name_extraspace(np);
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG,
                          lexeme_textpos_get(lex));
    expr_seg_name_set(exp, lexeme_ctx_get(lex));
    expr_seg_base_set(exp, sym->seg);
    expr_seg_units_set(exp, machine_scalar_units(mach));
    expr_seg_signext_set(exp, machine_addr_signed(mach));
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        exp = expr_parse_arglist(ctx, exp);
    }

    return exp;

} /* bind_routine */

/*
 * symbols_init
 */
void
symbols_init (expr_ctx_t ctx)
{

    int i;
    namectx_t namectx = scope_namectx(parser_scope_get(expr_parse_ctx(ctx)));
    symctx_t symctx = malloc(sizeof(struct symctx_s));
    machinedef_t *mach = expr_machinedef(ctx);

    static nametype_vectors_t symvec[3] = {
        { sizeof(sym_literal_t), 0, 0, 0, 0 },
        { sizeof(sym_data_t), 0, 0, data_free, data_copy },
//        { sizeof(sym_routine_t), 0, 0, routine_free, 0 }
    };

    memset(symctx, 0, sizeof(struct symctx_s));
    symctx->data_defaults.units = machine_scalar_units(mach);
    symctx->literal_defaults.width = machine_scalar_bits(mach);
    symctx->stg = expr_stg_ctx(ctx);
    nametables_symctx_set(namectx, symctx);
    for (i = 0; i < sizeof(symvec)/sizeof(symvec[0]); i++)
        nametype_dataop_register(namectx, symtype[i], &symvec[i], ctx);
    
    lextype_register(LEXTYPE_NAME_COMPILETIME, bind_compiletime);
    lextype_register(LEXTYPE_NAME_LITERAL, bind_literal);
    expr_dispatch_register(ctx, LEXTYPE_NAME_DATA, bind_data);
    expr_dispatch_register(ctx, LEXTYPE_NAME_ROUTINE, bind_routine);
    
} /* symbols_init */

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
}

static int
compare_data_attrs (data_attr_t *a, data_attr_t *b) {

    nameref_t *aref, *bref;

    if (a->units != b->units ||
        a->struc != b->struc ||
        a->psect != b->psect ||
        a->flags != b->flags ||
        namereflist_length(&a->fields) != namereflist_length(&b->fields)) {
        return 0;
    }

    // XXX this is not very efficient
    for (aref = namereflist_head(&a->fields); aref != 0; aref = aref->tq_next) {
        for (bref = namereflist_head(&b->fields);
             bref->np != aref->np && bref != 0; bref = bref->tq_next);
        if (bref == 0) return 0;
    }

    return 1;
}

name_t *
datasym_declare (scopectx_t scope, strdesc_t *dsc, symscope_t sc,
                 data_attr_t *attrp, textpos_t pos)
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
    }

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_DATA;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(scope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_DATA, &gsym);
        if (gnp != 0 && gsym->seg != 0 && !(attrp->flags & SYM_M_PENDING)) {
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
        memcpy(&sym->attr, attrp, sizeof(data_attr_t));
        if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
    }

    return np;
}

int
datasym_attr_update (name_t *np, data_attr_t *attrp)
{
    sym_data_t *sym = name_extraspace(np);
    sym_data_t *gsym = 0;

    if (sym->globalsym != 0) {
        gsym = name_extraspace(sym->globalsym);
        if (!(gsym->attr.flags & SYM_M_PENDING) &&
            !compare_data_attrs(&gsym->attr, attrp)) {
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(data_attr_t));
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(data_attr_t));
    }
    return 1;
}

data_attr_t *
datasym_attr (name_t *np) {
    sym_data_t *sym = name_extraspace(np);
    return &sym->attr;
}

unsigned long
datasym_segsize (name_t *np)
{
    sym_data_t *sym = name_extraspace(np);
    if (sym->seg == 0) {
        return 0;
    } else {
        return seg_size(sym->seg);
    }
}

seg_t *
datasym_seg (name_t *np)
{
    sym_data_t *sym = name_extraspace(np);
    return sym->seg;
}

void
datasym_seg_set (name_t *np, seg_t *seg)
{
    sym_data_t *sym = name_extraspace(np);
    sym->seg = seg;
    if (sym->globalsym != 0) {
        sym_data_t *gsym = name_extraspace(sym->globalsym);
        if (gsym->seg == 0) gsym->seg = seg;
    }
}

name_t *
compiletime_declare (scopectx_t scope, strdesc_t *dsc, long val, textpos_t pos)
{
    namedef_t ndef;
    name_t *np;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_COMPILETIME;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    np = name_declare(scope, &ndef, pos, 0, 0, 0);
    if (np != 0) name_value_signed_set(np, val);
    return np;

}
void compiletime_assign (name_t *np, long val) { name_value_signed_set(np, val); }
long compiletime_value (name_t *np) { return name_value_signed(np); }

name_t *
label_declare (scopectx_t scope, strdesc_t *dsc, textpos_t pos)
{
    namedef_t ndef;

    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_LABEL;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    return name_declare(scope, &ndef, pos, 0, 0, 0);

}
name_t *
litsym_search (scopectx_t scope, strdesc_t *dsc)
{
    return name_search_typed(scope, dsc->ptr, dsc->len, LEXTYPE_NAME_LITERAL, 0);
}
name_t *
litsym_declare (scopectx_t scope, strdesc_t *dsc, symscope_t sc,
                literal_attr_t *attrp, textpos_t pos)
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
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
        name_t *gnp = 0;
        sym_literal_t *gsym;
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(scope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_LITERAL, &gsym);
        if (gnp != 0) {
            if (gsym->attr.width != attrp->width ||
                (gsym->attr.flags & SYM_M_SIGNEXT)
                != (attrp->flags & SYM_M_SIGNEXT)) {
                return 0;
            }
        }
        if (gnp == 0) {
            gnp = name_declare(gscope, &ndef, pos, 0, 0, &gsym);
            if (gsym != 0) memcpy(&gsym->attr, attrp, sizeof(literal_attr_t));
        }
    }
    np = name_declare(scope, &ndef, pos, 0, 0, &sym);
    if (sym != 0) memcpy(&sym->attr, attrp, sizeof(literal_attr_t));
    return np;
}
name_t *
litsym_special (scopectx_t scope, strdesc_t *dsc, unsigned int value)
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
}

name_t *
rtnsym_search (scopectx_t scope, strdesc_t *namedsc)
{
    return name_search_typed(scope, namedsc->ptr, namedsc->len,
                           LEXTYPE_NAME_ROUTINE, 0);
}

static int
compare_routine_attrs (routine_attr_t *a, routine_attr_t *b) {

    if ((a->flags & SYM_M_NOVALUE) != (b->flags & SYM_M_NOVALUE)) {
        return 0;
    }

    return 1;
}

name_t *
rtnsym_declare (scopectx_t scope, strdesc_t *dsc, symscope_t sc,
                 routine_attr_t *attrp, textpos_t pos)
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
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(scope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_DATA, &gsym);
        if (gnp != 0 && gsym->seg != 0 && !(attrp->flags & SYM_M_PENDING)) {
            if (!compare_routine_attrs(&gsym->attr, attrp)) {
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
        if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
    }

    return np;
}

int
rtnsym_attr_update (name_t *np, routine_attr_t *attrp)
{
    sym_routine_t *sym = name_extraspace(np);
    sym_routine_t *gsym = 0;

    if (sym->globalsym != 0) {
        gsym = name_extraspace(sym->globalsym);
        if (!(gsym->attr.flags & SYM_M_PENDING) &&
            !compare_routine_attrs(&gsym->attr, attrp)) {
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(routine_attr_t));
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(routine_attr_t));
    }
    return 1;
}

routine_attr_t *
rtnsym_attr (name_t *np) {
    sym_routine_t *sym = name_extraspace(np);
    return &sym->attr;
}

seg_t *
rtnsym_seg (name_t *np)
{
    sym_routine_t *sym = name_extraspace(np);
    return sym->seg;
}

void
rtnsym_seg_set (name_t *np, seg_t *seg)
{
    sym_routine_t *sym = name_extraspace(np);
    sym->seg = seg;
    if (sym->globalsym) {
        sym_routine_t *gsym = name_extraspace(sym->globalsym);
        gsym->seg = seg;
    }
}

expr_node_t *
rtnsym_expr (name_t *np)
{
    sym_routine_t *sym = name_extraspace(np);
    return sym->rtnexp;
}

void
rtnsym_expr_set (name_t *np, expr_node_t *exp)
{
    sym_routine_t *sym = name_extraspace(np);
    sym->rtnexp = exp;
    if (sym->globalsym) {
        sym_routine_t *gsym = name_extraspace(sym->globalsym);
        gsym->rtnexp = exp;
    }
}
name_t *
psect_declare (scopectx_t scope, strdesc_t *dsc,
               unsigned int psflags, textpos_t pos)
{
    namectx_t namectx = scope_namectx(scope);
    symctx_t symctx = nametables_symctx_get(namectx);
    namedef_t ndef;
    psect_t *psect;
    name_t *np;

    psect = psect_create(symctx->stg, dsc, pos, psflags);
    if (psect == 0) {
        return 0;
    }
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_PSECT;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    np = name_declare(scope, &ndef, pos, 0, 0, 0);
    if (np != 0) name_value_pointer_set(np, psect);
    return np;

}

psect_t *
psect_search (scopectx_t scope, strdesc_t *dsc)
{
    name_t *np;
    np = name_search_typed(scope, dsc->ptr, dsc->len, LEXTYPE_NAME_PSECT, 0);
    if (np == 0) return 0;
    return name_value_pointer(np);
}
psect_t *psect_pointer (name_t *np) { return name_value_pointer(np); }

int
sym_undeclare (scopectx_t scope, strdesc_t *dsc)
{
    lextype_t type;
    name_t *np = name_search(scope, dsc->ptr, dsc->len, &type);

    if (np == 0 || (type < LEXTYPE_NAME_MIN || type > LEXTYPE_NAME_MAX)) {
        return 0;
    }
    return name_undeclare(scope, np);

}
