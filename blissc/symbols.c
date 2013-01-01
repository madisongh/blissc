/*
 *++
 *	File:			symbols.c
 *
 *	Abstract:		Symbol management.
 *
 *  Module description:
 *		This module contains the routines that manage information
 *		about symbols: LITERALs, data symbols, and routines.  These
 *		are usually just called 'names' in the LRM, but I use 'symbol'
 *		to distinguish these names from names of other things.
 *		COMPILETIME names are also handled in this module, even though
 *		they are purely lexical entities.  LABELs are also managed
 *      here.
 *
 *		This module layers on top of the generic name table management
 *		routines, which provide the underlying memory management and
 *		name lookup mechanisms.  See nametable.c for further information.
 *		Extension space is used for literals, routines, and data
 *      symbols.  COMPILETIME names make use of the generic 'value'
 *		storage already provided by the name table code.
 *
 *		The parsing routines in declarations.c call on these routines
 *		to add symbols to the symbol table, once the names and attributes
 *		have been parsed.  A mechanism is provided to partially declare
 *		a symbol, marking it "pending" until all attributes have been
 *		parsed and the symbol information gets updated.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *
 *	Modification history:
 *		20-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include "symbols.h"
#include "structures.h"
#include "nametable.h"
#include "lexeme.h"
#include "gencode.h"
#include "expression.h"
#include "storage.h"
#include "strings.h"
#include "utils.h"

/*
 * The following structures are the extensions to
 * the basic name type that hold the information specific
 * to the types of symbols tracked here.
 */
struct sym_literal_s {
    void           *genref;
    literal_attr_t attr;
};
typedef struct sym_literal_s sym_literal_t;

struct sym_label_s {
    expr_node_t     *block;
    void            *genref;
    void            *exitpoint;
};
typedef struct sym_label_s sym_label_t;

struct sym_data_s {
    name_t          *globalsym;
    seg_t           *seg;
    void            *genref;
    data_attr_t      attr;
};
typedef struct sym_data_s sym_data_t;

struct sym_routine_s {
    name_t          *globalsym;
    expr_node_t     *rtnexp;
    frame_t         *stack;
    seg_t           *seg;
    void            *genref;
    scopectx_t       argscope;
    routine_attr_t   attr;
};
typedef struct sym_routine_s sym_routine_t;

struct sym_module_s {
    strdesc_t       *ident;
    strdesc_t       *mainrtn;
    void            *genref;
    expr_node_t     *modblock;
};
typedef struct sym_module_s sym_module_t;

/*
 * Context structure for this module.
 */
struct symctx_s {
    logctx_t        logctx;
    gencodectx_t    gctx;
    stgctx_t        stg;
    data_attr_t     data_defaults;
    routine_attr_t  routine_defaults;
    literal_attr_t  literal_defaults;
};
typedef struct symctx_s *symctx_t;

static const lextype_t symtype[5] = {
    LEXTYPE_NAME_LITERAL, LEXTYPE_NAME_DATA,
    LEXTYPE_NAME_ROUTINE, LEXTYPE_NAME_MODULE,
    LEXTYPE_NAME_LABEL
};

/*
 * data_free
 *
 * Destructor function for data symbols.
 * Invoked through name_free().
 */
static void
data_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    sym_data_t *d = p;

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
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    sym_data_t *dsym = dp;
    sym_data_t *ssym = sp;
    nameref_t *ref;

    dsym->globalsym = ssym->globalsym;
    dsym->seg = ssym->seg;
    dsym->genref = ssym->genref;
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
    expr_ctx_t ctx = vctx;
    sym_module_t *m = p;

    if (m->ident != 0) string_free(expr_strctx(ctx), m->ident);
    if (m->mainrtn != 0) string_free(expr_strctx(ctx), m->mainrtn);

} /* module_free */

/*
 * module_copy
 *
 * Copy-constructor for a module cell.
 */
static int
module_copy (void *vctx, name_t *dnp, void *dp, name_t *snp, void *sp)
{
    expr_ctx_t ctx = vctx;
    sym_module_t *dm = dp;
    sym_module_t *sm = sp;

    if (sm->ident != 0) {
        dm->ident = string_copy(expr_strctx(ctx), 0, sm->ident);
        if (dm->ident ==  0) return 0;
    }
    if (sm->mainrtn != 0) {
        dm->mainrtn = string_copy(expr_strctx(ctx), 0, sm->mainrtn);
        if (dm->mainrtn == 0) return 0;
    }

    dm->genref = sm->genref;
    
    return 1;

} /* module_copy */

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
        expr_seg_units_set(exp, sym->attr.units);
        expr_seg_signext_set(exp, (sym->attr.flags & SYM_M_SIGNEXT) != 0);
        expr_is_ltce_set(exp, (sym->seg == 0 ? 0 : seg_addr_is_ltce(sym->seg)));
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
    expr_seg_units_set(exp, machine_scalar_units(mach));
    expr_seg_signext_set(exp, machine_addr_signed(mach));
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
 */
void
symbols_init (expr_ctx_t ctx, gencodectx_t gctx)
{

    int i;
    namectx_t namectx = scope_namectx(parser_scope_get(expr_parse_ctx(ctx)));
    symctx_t symctx = malloc(sizeof(struct symctx_s));
    lexctx_t lctx = expr_lexmemctx(ctx);
    machinedef_t *mach = expr_machinedef(ctx);

    static nametype_vectors_t symvec[5] = {
        { sizeof(sym_literal_t), 0, 0, 0, 0 },
        { sizeof(sym_data_t), 0, 0, data_free, data_copy },
        { sizeof(sym_routine_t), 0, 0, 0, 0 },
        { sizeof(sym_module_t), 0, 0, module_free, module_copy },
        { sizeof(sym_label_t), 0, 0, 0, 0 }
    };

    memset(symctx, 0, sizeof(struct symctx_s));
    symctx->logctx = expr_logctx(ctx);
    symctx->gctx = gctx;
    symctx->data_defaults.units = machine_scalar_units(mach);
    symctx->literal_defaults.width = machine_scalar_bits(mach);
    symctx->stg = expr_stg_ctx(ctx);
    nametables_symctx_set(namectx, symctx);
    for (i = 0; i < sizeof(symvec)/sizeof(symvec[0]); i++)
        nametype_dataop_register(namectx, symtype[i], &symvec[i], ctx);

    lextype_register(lctx, ctx, LEXTYPE_NAME_COMPILETIME, bind_compiletime);
    lextype_register(lctx, ctx, LEXTYPE_NAME_LITERAL, bind_literal);
    expr_dispatch_register(ctx, LEXTYPE_NAME_DATA, bind_data);
    expr_dispatch_register(ctx, LEXTYPE_NAME_ROUTINE, bind_routine);

} /* symbols_init */

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
        a->struc != b->struc ||
        a->psect != b->psect ||
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
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    if (attrp->flags & SYM_M_FORWARD) {
        ndef.flags |= NAME_M_FORWARD;
    }

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(gscope, dsc->ptr, dsc->len,
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
        if (sym->attr.flags & SYM_M_FORWARD) {
            if (attrp->flags & SYM_M_FORWARD) {
                log_signal(symctx->logctx, pos, STC__REDECLARE, dsc->ptr, dsc->len);
            } else if (!(attrp->flags & SYM_M_PENDING) &&
                       !compare_data_attrs(&sym->attr, attrp)) {
                log_signal(symctx->logctx, pos, STC__ATTRNCMPT, dsc);
            }
        }
        memcpy(&sym->attr, attrp, sizeof(data_attr_t));
        if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
        if (!(attrp->flags & SYM_M_PENDING) && symctx->gctx != 0) {
            gencode_datasym(symctx->gctx, np);
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
            symctx_t symctx = nametables_symctx_get(scope_namectx(name_scope(np)));
            log_signal(symctx->logctx, name_defpos(np), STC__ATTRNCMPT, name_string(np));
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(data_attr_t));
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(data_attr_t));
    }
    if (!(attrp->flags & SYM_M_PENDING) && symctx->gctx != 0) {
        gencode_datasym(symctx->gctx, np);
    }
    return 1;

} /* datasym_attr_update */

/*
 * Getter/setter routines for data symbols
 *
 */
data_attr_t *datasym_attr (name_t *np) { sym_data_t *sym = name_extraspace(np);
    return &sym->attr; }
unsigned long datasym_segsize (name_t *np) { sym_data_t *sym = name_extraspace(np);
    return (sym->seg == 0 ? 0 : seg_size(sym->seg)); }
seg_t * datasym_seg (name_t *np) { sym_data_t *sym = name_extraspace(np); return sym->seg; }
void datasym_seg_set (name_t *np, seg_t *seg) { sym_data_t *sym = name_extraspace(np);
    sym->seg = seg; if (sym->globalsym != 0) datasym_seg_set(sym->globalsym, seg); }
void *datasym_genref (name_t *np) { sym_data_t *sym = name_extraspace(np);
    return sym->genref; }
void datasym_genref_set (name_t *np, void *ref) { sym_data_t *sym = name_extraspace(np);
    sym->genref = ref; }

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

expr_node_t *label_block (name_t *np) { sym_label_t *lbl = name_extraspace(np);
    return lbl->block;
}
void label_block_set (name_t *np, expr_node_t *exp) {
    sym_label_t *lbl = name_extraspace(np); lbl->block = exp;
}
void *label_genref (name_t *np) { sym_label_t *lbl = name_extraspace(np);
    return lbl->genref;
}
void label_genref_set (name_t *np, void *p) {
    sym_label_t *lbl = name_extraspace(np); lbl->genref = p; }
void *label_exitpoint (name_t *np) { sym_label_t *lbl = name_extraspace(np);
    return lbl->exitpoint; }
void label_exitpoint_set (name_t *np, void *p) {
    sym_label_t *lbl = name_extraspace(np); lbl->exitpoint = p; }


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
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    ndef.flags |= (attrp->flags & SYM_M_RESERVED) ? NAME_M_RESERVED : 0;

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
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
        if (symctx->gctx != 0) {
            gencode_litsym(symctx->gctx, np);
        }
    }
    return np;

} /* litsym_declare */

literal_attr_t *litsym_attr (name_t *np) { sym_literal_t *sym = name_extraspace(np);
    return &sym->attr; }
void *litsym_genref (name_t *np) { sym_literal_t *sym = name_extraspace(np);
    return sym->genref; }
void litsym_genref_set (name_t *np, void *ref) { sym_literal_t *sym = name_extraspace(np);
    sym->genref = ref; }

/*
 * litsym_special
 *
 * For internal use by the compiler in appropriate circumstances
 * only, this routine is for defining LITERALs in a local name
 * scope only, and overrides the normal checks for user-defined
 * names.
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
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    if (attrp->flags & SYM_M_FORWARD) {
        ndef.flags |= NAME_M_FORWARD;
    }

    if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
        scopectx_t gscope = nametables_globalscope(namectx);
        gnp = name_search_typed(gscope, dsc->ptr, dsc->len,
                                LEXTYPE_NAME_ROUTINE, &gsym);
        if (gnp != 0 && gsym->seg != 0 && !(attrp->flags & SYM_M_PENDING)) {
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
        // It is OK to redeclare EXTERNAL ROUTINEs, I guess
//        if ((sym->attr.flags & SYM_M_FORWARD) || sc == SYMSCOPE_EXTERNAL) {
//           if ((attrp->flags & SYM_M_PENDING) == 0 &&
//                       !compare_routine_attrs(&sym->attr, attrp)) {
//                log_signal(symctx->logctx, pos, STC__ATTRNCMPT, dsc);
//            }
//        }
        memcpy(&sym->attr, attrp, sizeof(routine_attr_t));
        if (sc == SYMSCOPE_EXTERNAL || sc == SYMSCOPE_GLOBAL) {
            sym->globalsym = gnp;
        }
        if (!(sym->attr.flags & SYM_M_PENDING) && symctx->gctx != 0) {
            gencode_rtnsym(symctx->gctx, np);
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
            symctx_t symctx = nametables_symctx_get(scope_namectx(name_scope(np)));
            log_signal(symctx->logctx, name_defpos(np), STC__ATTRNCMPT, name_string(np));
            return 0;
        }
    }
    memcpy(&sym->attr, attrp, sizeof(routine_attr_t));
    if (gsym != 0 && (gsym->attr.flags & SYM_M_PENDING)) {
        memcpy(&gsym->attr, attrp, sizeof(routine_attr_t));
    }
    if (!(sym->attr.flags & SYM_M_PENDING) && symctx->gctx != 0) {
        gencode_rtnsym(symctx->gctx, np);
    }
    return 1;

} /* rtnsym_attr_update */

/*
 * Getter/setter functions for routine symbols
 */
routine_attr_t * rtnsym_attr (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return &sym->attr; }
seg_t * rtnsym_seg (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return sym->seg; }
void rtnsym_seg_set (name_t *np, seg_t *seg) { sym_routine_t *sym = name_extraspace(np);
    sym->seg = seg; if (sym->globalsym) rtnsym_seg_set(sym->globalsym, seg); }
expr_node_t *rtnsym_expr (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return sym->rtnexp; }
void rtnsym_expr_set (name_t *np, expr_node_t *exp) { sym_routine_t *sym = name_extraspace(np);
    sym->rtnexp = exp; if (sym->globalsym) rtnsym_expr_set(sym->globalsym, exp); }
void * rtnsym_genref (name_t *np) { sym_routine_t *sym = name_extraspace(np);
    return sym->genref; }
void rtnsym_genref_set (name_t *np, void *ref) { sym_routine_t *sym = name_extraspace(np);
    sym->genref = ref; }

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
    psect_t *psect;
    name_t *np;

    psect = psect_create(symctx->stg, dsc, pos, psflags);
    if (psect == 0) {
        return 0;
    }
    memset(&ndef, 0, sizeof(ndef));
    ndef.lt = LEXTYPE_NAME_PSECT;
    ndef.flags = NAME_M_DECLARED;
    ndef.name = dsc->ptr;
    ndef.namelen = dsc->len;
    np = name_declare(scope, &ndef, pos, 0, 0, 0);
    if (np != 0) name_value_pointer_set(np, psect);
    return np;

} /* psect_declare */

/*
 * psect_search
 *
 * Looks up a PSECT by name.
 */
psect_t *
psect_search (scopectx_t scope, strdesc_t *dsc)
{
    name_t *np;
    np = name_search_typed(scope, dsc->ptr, dsc->len, LEXTYPE_NAME_PSECT, 0);
    if (np == 0) return 0;
    return name_value_pointer(np);

} /* psect_search */

/*
 * psect_pointer
 *
 * Returns the pointer to the psect tracking structure,
 * given a name pointer (for an already-looked-up PSECT name).
 */
psect_t *psect_pointer (name_t *np) { return name_value_pointer(np); }

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
static int
get_sym_base (name_t *np, psect_t **psp, name_t **extsymp)
{
    sym_data_t *d = name_extraspace(np);
    sym_routine_t *r = name_extraspace(np);

    *psp = 0;
    *extsymp = 0;
    switch (name_type(np)) {
        case LEXTYPE_NAME_DATA:
            if (d->seg != 0) {
                if (seg_type(d->seg) == SEGTYPE_STATIC) {
                    *psp = seg_static_psect(d->seg);
                    return 1;
                }
                return 0;
            }
            if (d->globalsym != 0) {
                *extsymp = d->globalsym;
                return 1;
            }
            break;

        case LEXTYPE_NAME_ROUTINE:
            if (r->seg != 0) {
                if (seg_type(r->seg) == SEGTYPE_STATIC) {
                    *psp = seg_static_psect(r->seg);
                    return 1;
                }
                return 0;
            }
            if (r->globalsym != 0) {
                *extsymp = r->globalsym;
                return 1;
            }
            break;

        default:
            break;
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
    name_t *ext_a, *ext_b;
    psect_t *ps_a, *ps_b;

    if (!get_sym_base(np_a, &ps_a, &ext_a) ||
        !get_sym_base(np_b, &ps_b, &ext_b)) {
        return 0;
    }
    if (ps_a != 0) {
        return ps_a == ps_b;
    }
    if (ext_a != 0) {
        return ext_a == ext_b;
    }
    return 0;

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