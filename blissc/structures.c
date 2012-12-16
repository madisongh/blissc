//
//  structures.c
//  blissc
//
//  STRUCTURE and FIELD definitions.
//
//  Created by Matthew Madison on 11/28/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include "declarations.h"
#include "structures.h"
#include "symbols.h"
#include "macros.h"
#include "parser.h"
#include "expression.h"
#include "nametable.h"
#include "lexeme.h"

struct strudef_s {
    scopectx_t acctbl, allotbl;
    namereflist_t accformals;
    namereflist_t alloformals;
    lexseq_t accbody, allobody;
};

struct pdclinfo_s {
    char *lines[4];
    int current;
};

static lextype_t delims[] = { LEXTYPE_DELIM_RBRACK, LEXTYPE_DELIM_SEMI };
static lextype_t bodyends[] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_SEMI };
static strdesc_t leftparen = STRDEF("(");
static strdesc_t rightparen = STRDEF(")");
static strdesc_t dot = STRDEF(".");

static char *predeclared_bitvector =
    "STRUCTURE BITVECTOR[I;N] = "
    "[(N+(%BPUNIT-1)/%BPUNIT](BITVECTOR+I/%BPUNIT)<I MOD %BPUNIT,1,0>;";
static char *predeclared_vector_u_s =
    "STRUCTURE VECTOR [I;N,UNIT=%UPVAL,EXT=0] ="
    "[N*UNIT](VECTOR+I*UNIT)<0,%BPUNIT*UNIT,EXT>;";
static char *predeclared_vector_u_ns =
    "STRUCTURE VECTOR [I;N,UNIT=%UPVAL] = [N*UNIT](VECTOR+I*UNIT)<0,%BPUNIT*UNIT>;";
static char *predeclared_vector_nu_ns =
    "STRUCTURE VECTOR [I;N] = [N](VECTOR+I)<0,%BPUNIT>;";
static char *predeclared_vector_nu_s =
    "STRUCTURE VECTOR [I;N,EXT=0] = [N](VECTOR+I)<0,%BPUNIT,EXT>;";
static char *predeclared_block_u =
    "STRUCTURE BLOCK[O,P,S,E;BS,UNIT=%UPVAL] = [BS*UNIT](BLOCK+O*UNIT)<P,S,E>;";
static char *predeclared_block_nu =
    "STRUCTURE BLOCK[O,P,S,E;BS] = [BS](BLOCK+O)<P,S,E>;";
static char *predeclared_blockvector_u =
    "STRUCTURE BLOCKVECTOR[I,O,P,S,E;N,BS,UNIT=%UPVAL] = "
    "[N*BS*UNIT](BLOCKVECTOR+(I*BS+O)*UNIT)<P,S,E>;";
static char *predeclared_blockvector_nu =
    "STRUCTURE BLOCKVECTOR[I,O,P,S,E;N,BS] = "
    "[N*BS](BLOCKVECTOR+(I*BS+O)<P,S,E>;";

/*
 * structure_bind
 */
expr_node_t *
structure_bind (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    name_t *np = lexeme_ctx_get(lex);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
        return 0;
    }

    return structure_reference(ctx, np, 0, 0, lex);

} /* structure_bind */

static int
structure_init (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    strudef_t *stru = p;
    namectx_t namectx = expr_namectx(ctx);

    stru->acctbl = scope_begin(namectx, 0);
    stru->allotbl = scope_begin(namectx, 0);
    namereflist_init(&stru->accformals);
    namereflist_init(&stru->alloformals);
    lexseq_init(&stru->accbody);
    lexseq_init(&stru->allobody);
    return 1;
}

static void
structure_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    lexctx_t lctx = expr_lexmemctx(ctx);
    strudef_t *stru = p;
    namectx_t namectx = expr_namectx(ctx);

    scope_end(stru->acctbl);
    scope_end(stru->allotbl);
    namereflist_free(namectx, &stru->accformals);
    namereflist_free(namectx, &stru->alloformals);
    lexseq_free(lctx, &stru->accbody);
    lexseq_free(lctx, &stru->allobody);

} /* structure_free */

static int
structure_copy (void *vctx, name_t *dst, void *dp,
                name_t *src, void *sp)
{
    expr_ctx_t ctx = vctx;
    lexctx_t lctx = expr_lexmemctx(ctx);
    strudef_t *dstru = dp;
    strudef_t *sstru = sp;
    namectx_t namectx = expr_namectx(ctx);
    nameref_t *ref;
    name_t *np;

    dstru->acctbl = scope_copy(sstru->acctbl, 0);
    dstru->allotbl = scope_copy(sstru->allotbl, 0);
    for (ref = namereflist_head(&sstru->accformals); ref != 0;
         ref = ref->tq_next) {
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *str = name_string(ref->np);
            np = name_search(dstru->acctbl, str->ptr, str->len, 0);
        }
        namereflist_instail(&dstru->accformals, nameref_alloc(namectx, np));
    }
    for (ref = namereflist_head(&sstru->alloformals); ref != 0;
         ref = ref->tq_next) {
        if (ref->np == 0) {
            np = 0;
        } else {
            strdesc_t *str = name_string(ref->np);
            np = name_search(dstru->allotbl, str->ptr, str->len, 0);
        }
        namereflist_instail(&dstru->alloformals, nameref_alloc(namectx, np));
    }
    lexseq_copy(lctx, &dstru->accbody, &sstru->accbody);
    lexseq_copy(lctx, &dstru->allobody, &sstru->allobody);

    return 1;
    
} /* structure_copy */

static int
field_init (void *vctx, name_t *np, void *p)
{
    lexseq_t *fseq = p;

    lexseq_init(fseq);

    return 1;

} /* field_init */

static void
field_free (void *vctx, name_t *np, void *p)
{
    lexseq_t *fseq = p;

    lexseq_free(expr_lexmemctx(vctx), fseq);

} /* field_free */

static int
field_copy (void *vctx, name_t *dst, void *dp,
                name_t *src, void *sp)
{
    lexseq_t *dseq = dp;
    lexseq_t *sseq = sp;

    return lexseq_copy(expr_lexmemctx(vctx), dseq, sseq);

} /* field_copy */

lexseq_t *field_lexseq (name_t *fnp) { return name_extraspace(fnp); }

lexeme_t *
field_extract (name_t *fnp, unsigned int which)
{
    lexseq_t *seq = name_extraspace(fnp);
    lexeme_t *lex;
    unsigned int i;

    for (i = 0, lex = lexseq_head(seq); i < which && lex != 0;
         i++, lex = lexeme_next(lex)) {
        lex = lexeme_next(lex);
        if (lex == 0) break;
    }
    if (lex == 0) return 0;
    return lex;
}

static int
fieldset_init (void *vctx, name_t *np, void *p)
{
    namereflist_t *refs = p;

    namereflist_init(refs);

    return 1;

} /* fieldset_init */

static void
fieldset_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    namereflist_t *refs = p;

    namereflist_free(namectx, refs);

} /* fieldset_free */

static int
fieldset_copy (void *vctx, name_t *dst, void *dp,
            name_t *src, void *sp)
{
    namereflist_t *drefs = dp;
    namereflist_t *srefs = sp;
    expr_ctx_t ctx = vctx;
    scopectx_t dscope = name_scope(dst);
    namectx_t namectx = expr_namectx(ctx);
    nameref_t *ref;
    name_t *np;

    for (ref = namereflist_head(srefs); ref != 0; ref = ref->tq_next) {
        if (ref->np == 0) {
            // XXX should never happen
            continue;
        }
        strdesc_t *str = name_string(ref->np);
        np = name_search(dscope, str->ptr, str->len, 0);
        namereflist_instail(drefs, nameref_alloc(namectx, np));
    }

    return 1;

} /* fieldset_copy */

namereflist_t *fieldset_reflist (name_t *fsnp) { return name_extraspace(fsnp); }

static int
predeclare_structures (void *myctx, char *buf, size_t bufsiz, size_t *lenp)
{
    struct pdclinfo_s *ctx = myctx;
    size_t len;

    if (ctx->current >= sizeof(ctx->lines)/sizeof(ctx->lines[0])
        || ctx->lines[ctx->current] == 0) {
        return 0; // EOF
    }
    len = strlen(ctx->lines[ctx->current]);
    if (len >= bufsiz) {
        len = bufsiz-1;
    }
    memcpy(buf, ctx->lines[ctx->current], len);
    *lenp = len;
    ctx->current += 1;
    return 1;
}

/*
 * structures_init
 */
void
structures_init (expr_ctx_t ctx)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    struct pdclinfo_s pdinfo;
    machinedef_t *mach = expr_machinedef(ctx);
    int nounits = (machine_scalar_units(mach) == 1);
    int signext = machine_signext_supported(mach);
    nametype_vectors_t vec;
    int i;

    expr_dispatch_register(ctx, LEXTYPE_NAME_STRUCTURE, structure_bind);

    memset(&vec, 0, sizeof(vec));
    vec.typesize = sizeof(strudef_t);
    vec.typeinit = structure_init;
    vec.typefree = structure_free;
    vec.typecopy = structure_copy;
    nametype_dataop_register(namectx, LEXTYPE_NAME_STRUCTURE, &vec, ctx);
    vec.typesize = sizeof(lexseq_t);
    vec.typeinit = field_init;
    vec.typefree = field_free;
    vec.typecopy = field_copy;
    nametype_dataop_register(namectx, LEXTYPE_NAME_FIELD, &vec, ctx);
    vec.typesize = sizeof(namereflist_t);
    vec.typeinit = fieldset_init;
    vec.typefree = fieldset_free;
    vec.typecopy = fieldset_copy;
    nametype_dataop_register(namectx, LEXTYPE_NAME_FIELDSET, &vec, ctx);

    pdinfo.current = 0;
    pdinfo.lines[0] = (nounits ? (signext ? predeclared_vector_nu_s
                                  : predeclared_vector_nu_ns)
                       : (signext ? predeclared_vector_u_s
                          : predeclared_vector_u_ns));
    pdinfo.lines[1] = predeclared_bitvector;
    pdinfo.lines[2] = (nounits ? predeclared_block_nu
                       : predeclared_block_u);
    pdinfo.lines[3] = (nounits ? predeclared_blockvector_nu
                       : predeclared_blockvector_u);

    parser_popen(pctx, predeclare_structures, &pdinfo);
    for (i = 0; i < 4; i++) {
        parse_declaration(ctx);
    }

} /* structures_init */

/*
 * declare_structure
 */
int
declare_structure (expr_ctx_t ctx, scopectx_t scope)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lextype_t term = LEXTYPE_DELIM_COMMA;
    strudef_t *stru;
    name_t *np;
    strdesc_t *struname;
    textpos_t pos;
    namedef_t ndef;
    int which;

    while (term == LEXTYPE_DELIM_COMMA) {
        if (!parse_decl_name(pctx, scope, &struname, &pos)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
        }
        memset(&ndef, 0, sizeof(ndef));
        ndef.lt = LEXTYPE_NAME_STRUCTURE;
        ndef.flags = NAME_M_DECLARED;
        ndef.name = struname->ptr;
        ndef.namelen = struname->len;
        np = name_declare(scope, &ndef, pos, 0, 0, &stru);
        if (np == 0) {
            /* XXX error condition */
        }
        which = macro_paramlist(pctx, 0, 1, 0, delims, 2,
                                &stru->acctbl, &stru->accformals);
        if (which < 0) {
            /* XXX error condition */
        } else if (which == 1) {
            which = macro_paramlist(pctx, 0, 1, 0, delims, 1,
                                    &stru->allotbl, &stru->alloformals);
            if (which < 0) {
                /* XXX error condition */
            }
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            /* XXX error condition */
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            parser_scope_begin(pctx);
            if (!parse_lexeme_seq(pctx, 0, QL_MACRO, delims, 1, &stru->allobody, 0)) {
                /* XXX error condition */
            }
            parser_scope_end(pctx);
        }
        parser_scope_begin(pctx);
        if (!parse_lexeme_seq(pctx, 0, QL_MACRO, bodyends, 2, &stru->accbody, &term)) {
            /* XXX error condition */
        }
        parser_scope_end(pctx);

    } /* while */

    return 1;
    
} /* declare_structure */

/*
 * parse_fields
 *
 * Recursive routine for defining individual fields
 * and field-sets.
 */
static int
parse_fields (expr_ctx_t ctx, scopectx_t scope, namereflist_t *fldset)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    strdesc_t *fldname;
    textpos_t fpos;
    lexseq_t *fseq;
    lexeme_t *lex;
    name_t *fldnp;
    namedef_t ndef;
    lextype_t delims[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RBRACK };
    int which;
    static strdesc_t zero = STRDEF("0");

    while (1) {
        if (!parse_decl_name(pctx, scope, &fldname, &fpos)) {
            /* XXX error condition */
            break;
        }
        memset(&ndef, 0, sizeof(ndef));
        ndef.name = fldname->ptr;
        ndef.namelen = fldname->len;
        ndef.flags = NAME_M_DECLARED;
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            /* XXX error condition */
        }
        // Check for fieldset and recurse
        if (fldset == 0 && parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
            namereflist_t *frefs;
            ndef.lt = LEXTYPE_NAME_FIELDSET;
            fldnp = name_declare(scope, &ndef, fpos, 0, 0, &frefs);
            if (fldnp == 0) {
                /* XXX error condition */
                break;
            }
            if (!parse_fields(ctx, scope, frefs)) {
                /* XXX error condition */
                return 0;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
                /* XXX error condition */
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                break;
            }
            continue; // set defined; move on to the next name
        }
        // Just a regular field
        ndef.lt = LEXTYPE_NAME_FIELD;
        fldnp = name_declare(scope, &ndef, fpos, 0, 0, &fseq);
        if (fldnp == 0) {
            /* XXX error condition */
            break;
        }
        if (fldset != 0) {
            namereflist_instail(fldset, nameref_alloc(namectx, fldnp));
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
        }
        while (1) {
            if (expr_parse_ctce(ctx, &lex)) {
                lexseq_instail(fseq, lex);
            } else {
                /* XXX error condition */
                lexseq_instail(fseq,
                               lexeme_create(parser_lexmemctx(pctx),
                                             LEXTYPE_NUMERIC, &zero));
            }
            which = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, &lex, 1);
            if (which != 0) {
                /* XXX error condition if < 0 */
                break;
            }
            lexseq_instail(fseq, lex);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    } /* while */

    return 1;

} /* parse_fields */

/*
 * declare_field
 */
int
declare_field (expr_ctx_t ctx, scopectx_t scope)
{
    if (parse_fields(ctx, scope, 0)) {
        return parser_expect(expr_parse_ctx(ctx), QL_NORMAL,
                             LEXTYPE_DELIM_SEMI, 0, 1);
    }
    return 0;

} /* declare_field */


/*
 * structure_allocate
 */
int
structure_allocate (expr_ctx_t ctx, name_t *struname,
                    strudef_t **strup, unsigned int *nunits,
                    scopectx_t *scopep)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    strudef_t *stru = name_extraspace(struname);
    machinedef_t *mach = expr_machinedef(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    int i;
    lexeme_t *lex;
    lexseq_t tmpseq;
    scopectx_t myscope, retscope;
    nameref_t *ref;
    name_t *np;
    static strdesc_t aus[4] = {
        STRDEF("BYTE"), STRDEF("WORD"), STRDEF("LONG"), STRDEF("QUAD") };
    static strdesc_t kw_signed = STRDEF("SIGNED");
    static strdesc_t kw_unsigned = STRDEF("UNSIGNED");

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
        *nunits = 0;
        return 1;
    }

    myscope = parser_scope_begin(pctx);
    if (scopep != 0) {
        retscope = scope_begin(scope_namectx(myscope), 0);
    }

    // Predeclare the allocation-unit names to their corresponding
    // values, but only for machines that have 8-bit units, and
    // only those names that correspond to a number of units that
    // is less than or equal to a fullword.
    if (machine_unit_bits(mach) == 8) {
        for (i = 0; i < 4; i++) {
            unsigned int units = 1<<i;
            if (machine_scalar_units(mach) < units) {
                break;
            }
            np = litsym_special(myscope, &aus[i], units);
            if (np == 0) {
                /* XXX error condition */
            }
        }
    }
    // and the sign-extension keywords, where supported
    if (machine_signext_supported(mach)) {
        np = litsym_special(myscope, &kw_signed, 1);
        if (np == 0) {
            /* XXX error condition */
        }
        np = litsym_special(myscope, &kw_unsigned, 0);
        if (np == 0) {
            /* XXX error condition */
        }
    }
    // Now fill in the default values for the allocation formals, if they
    // have any
    for (ref = namereflist_head(&stru->alloformals); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0) {
            strdesc_t *alloname = name_string(ref->np);
            lexseq_t seq;
            expr_node_t *exp;
            lexseq_init(&seq);
            lexseq_copy(lctx, &seq, macparam_lexseq(ref->np));
            if (lexseq_length(&seq) > 0 && expr_parse_seq(ctx, &seq, &exp)) {
                // XXX any CTCE should be good here
                if (expr_type(exp) == EXPTYPE_PRIM_LIT)
                    litsym_special(myscope, alloname,
                                   (unsigned int) expr_litval(exp));
                expr_node_free(ctx, exp);
            }
        }
    }
    // Now parse the allocation actuals
    for (ref = namereflist_head(&stru->alloformals); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0) {
            name_t *rnp;
            strdesc_t *alloname = name_string(ref->np);
            unsigned long val;
            if (expr_parse_ctce(ctx, &lex)) {
                val = lexeme_unsignedval(lex);
                lexeme_free(lctx, lex);
                np = litsym_special(myscope, alloname, val);
            } else {
                np = litsym_search(myscope, alloname, &val);
            } if (np == 0) {
                /* XXX error condition - missing and no default */
                val = 0;
                np = litsym_special(myscope, alloname, val);
                if (np == 0) {
                    /* XXX error condition */
                }
            }
            // Now copy the declaration into the scope we'll pass back
            // to the caller for later use
            if (scopep != 0) {
                rnp = litsym_special(retscope, alloname, val);
                if (rnp == 0) {
                    /* XXX error condition */
                }
            }
        }
        if (ref->tq_next != 0 &&
            !parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
        /* XXX error condition */
    }
    lexseq_init(&tmpseq);
    lexseq_copy(lctx, &tmpseq, &stru->allobody);
    parser_insert_seq(pctx, &tmpseq);
    if (!expr_parse_ctce(ctx, &lex)) {
        /* XXX error condition */
        return 0;
    }
    *nunits = (unsigned int)lexeme_unsignedval(lex);
    lexeme_free(lctx, lex);
    if (scopep != 0) *scopep = retscope;
    parser_scope_end(pctx);
    if (strup != 0) *strup = stru;

    return 1;

} /* structure_allocate */

/*
 * Common structure reference routine
 *
 * General structure reference
 * structure-name [ expression {,access-actual...} {; alloc-actual...} ]
 *
 * Ordinary structure reference
 * segment-name [ access-actual... ]
 *
 */
expr_node_t *
structure_reference (expr_ctx_t ctx, name_t *struname, int ctce_accessors,
                     name_t *symname, lexeme_t *curlex)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    lextype_t delim;
    lexseq_t seq;
    scopectx_t myscope, fldscope;
    nameref_t *ref;
    expr_node_t *exp, *resexp;
    textpos_t pos = lexeme_textpos_get(curlex);
    strudef_t *stru = name_extraspace(struname);
    static lextype_t delims[3] = { LEXTYPE_DELIM_RBRACK, LEXTYPE_DELIM_COMMA,
        LEXTYPE_DELIM_SEMI };
    int ndelims;

    fldscope = 0;
    lexseq_init(&seq);
    // If this is a general structure reference, get the
    // address expression
    if (symname == 0) {

        //
        //   XXX This needs fixing XXX
        //
        ndelims = 3;
        if (!parse_lexeme_seq(pctx, 0, QL_NORMAL, delims, 3, &seq, &delim)) {
            return 0;
        }
        myscope = scope_copy(stru->acctbl, 0);
    } else {
        data_attr_t *attr = datasym_attr(symname);
        if (attr == 0) {
            /* XXX error condition */
            return 0;
        }
        if (attr->flags & SYM_M_REF) {
            lexseq_instail(&seq,lexeme_create(lctx, LEXTYPE_DELIM_LPAR, &leftparen));
            lexseq_instail(&seq,lexeme_create(lctx, LEXTYPE_OP_FETCH, &dot));
        }
        lexseq_instail(&seq,lexeme_copy(lctx, curlex));
        if (attr->flags & SYM_M_REF) {
            lexseq_instail(&seq,lexeme_create(lctx, LEXTYPE_DELIM_RPAR, &rightparen));
        }
        // Semicolons (and allocation-formals) not allowed in this case
        ndelims = 2;
        delim = LEXTYPE_DELIM_COMMA;
        myscope = scope_copy(attr->struscope, 0);
        // Bring in the field names, so they can be used in the
        // the structure expression
        if (namereflist_length(&attr->fields) > 0) {
            fldscope = scope_begin(scope_namectx(myscope), 0);
            for (ref = namereflist_head(&attr->fields); ref != 0;
                 ref = ref->tq_next) {
                if (ref->np != 0) {
                    strdesc_t *fname = name_string(ref->np);
                    macparam_special(fldscope, fname, field_lexseq(ref->np));
                }
            }
        }
    }
    macparam_special(myscope, name_string(struname), &seq);
    lexseq_free(lctx, &seq);
    if (fldscope != 0) {
        parser_scope_push(pctx, fldscope);
    }
    for (ref = namereflist_head(&stru->accformals); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0) {
            strdesc_t *pname = name_string(ref->np);
            lexseq_init(&seq);
            if (delim == LEXTYPE_DELIM_COMMA) {
                if (!parse_lexeme_seq(pctx, 0, QL_NORMAL, delims, ndelims,
                                      &seq, &delim)) {
                    /* XXX error condition */
                    break;
                }
                if (ctce_accessors) {
                    lexseq_t testseq;
                    lexseq_init(&testseq);
                    lexseq_copy(lctx, &testseq, &seq);
                    // XXX another place where it would be convenient
                    // to have an expr_parse_seq_ctce() that returns a lexeme
                    if (!expr_parse_seq(ctx, &testseq, &exp) ||
                        expr_type(exp) != EXPTYPE_PRIM_LIT) {
                        /* XXX error condition */
                    } else {
                        expr_node_free(ctx, exp);
                    }
                    lexseq_free(lctx, &testseq);
                }
            }
            if (lexseq_length(&seq) == 0) {
                macparam_lookup(lctx, stru->acctbl, pname, &seq);
            }
            macparam_special(myscope, pname, &seq);
            lexseq_free(lctx, &seq);
        }
    }
    if (fldscope != 0) {
        parser_scope_end(pctx);
    }
    // Allocation-formals are only used in the general-reference case
    if (symname == 0) {
        for (ref = namereflist_head(&stru->alloformals); ref != 0;
             ref = ref->tq_next) {
            if (ref->np != 0) {
                strdesc_t *pname = name_string(ref->np);
                lexseq_init(&seq);
                if (delim != LEXTYPE_DELIM_RBRACK) {
                    if (!parse_lexeme_seq(pctx, 0, QL_NORMAL, delims, 3,
                                          &seq, &delim)) {
                        break;
                    }
                }
                if (lexseq_length(&seq) == 0) {
                    macparam_lookup(lctx, stru->allotbl, pname, &seq);
                }
                macparam_special(myscope, pname, &seq);
            }
        }
    }
    if (delim != LEXTYPE_DELIM_RBRACK) {
        /* XXX error condition */
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RBRACK);
    }
    lexseq_init(&seq);
    lexseq_copy(lctx, &seq, &stru->accbody);
    parser_scope_push(pctx, myscope);
    if (!expr_parse_seq(ctx, &seq, &exp)) {
        /* XXX error condition */
        lexseq_free(lctx, &seq);
        parser_scope_end(pctx);
        return 0;
    }

    parser_scope_end(pctx);
    resexp = expr_node_alloc(ctx, EXPTYPE_PRIM_STRUREF, pos);
    expr_struref_accexpr_set(resexp, exp);
    return exp;

} /* structure_reference */