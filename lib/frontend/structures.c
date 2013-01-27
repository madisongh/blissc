/*
 *++
 *	File:			structures.c
 *
 *	Abstract:		Structure and field handling.
 *
 *  Module description:
 *		This module handles structure declarations and references.
 *		Structures are implemented as a pair of macros, one for
 *		allocation and one for access, although without the specialized
 *		processing that macro expansion entails.
 *		Fields are also implemented as macros, and field sets are
 *		simply lists of references to field definitions.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		21-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdio.h>
#include "blissc/declarations.h"
#include "blissc/structures.h"
#include "blissc/symbols.h"
#include "blissc/macros.h"
#include "blissc/parser.h"
#include "blissc/expression.h"
#include "blissc/nametable.h"
#include "blissc/lexeme.h"

/*
 * Structure definition.  Contains
 * information about the access and
 * allocation parameters and the macro
 * bodies for each.
 */
struct strudef_s {
    scopectx_t acctbl, allotbl;
    namereflist_t accformals;
    namereflist_t alloformals;
    lexseq_t accbody, allobody;
};

/*
 * Some commonly-used data used throughout this module.
 */
static lextype_t bodyends[] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_SEMI };
static strdesc_t leftparen = STRDEF("(");
static strdesc_t rightparen = STRDEF(")");
static strdesc_t zero = STRDEF("0");
static namedef_t mynames[] = {
    NAMEDEF("%FIELDEXPAND", LEXTYPE_LXF_FIELDEXPAND, NAME_M_RESERVED),
    NAMEDEF("%SIZE", LEXTYPE_LXF_SIZE, NAME_M_RESERVED)
};

/*
 * The following definitions are used in the initialization
 * routine to set up the predeclared structures.  It was simpler
 * to provide these in text form and run them through the parser
 * than to set up the structures manually. There are different
 * flavors of these definitions, based on whether or not the
 * target machine allows for UNIT and EXT parameters.
 */
struct pdclinfo_s {
    char *lines[4];
    int current;
};

static char *predeclared_bitvector =
    "STRUCTURE BITVECTOR[I;N] = "
    "[(N+(%BPUNIT-1))/%BPUNIT](BITVECTOR+I/%BPUNIT)<I MOD %BPUNIT,1,0>;";
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

static int parse_FIELDEXPAND(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_SIZE(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);

/*
 * structure_bind
 *
 * Expression-binding routine for structures.  If the structure
 * name is followed by a left bracket, we have a structure reference
 * that must be expanded.  Otherwise, we leave the structure name
 * alone.
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

/*
 * structure_init
 *
 * Constructor for a structure cell extension
 * in a name-tracking structure.
 */
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

} /* structure_init */

/*
 * structure_free
 *
 * Destructor for a structure cell.
 */
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

/*
 * structure_copy
 *
 * Copy-constructor for a structure cell.  Handles
 * the fact that name references must point to the
 * newly-constructed scope tables, rather than the
 * originals.
 */
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

/*
 * field_init
 *
 * Field constructor.
 */
static int
field_init (void *vctx, name_t *np, void *p)
{
    lexseq_t *fseq = p;

    lexseq_init(fseq);

    return 1;

} /* field_init */

/*
 * field_free
 *
 * Field destructor.
 */
static void
field_free (void *vctx, name_t *np, void *p)
{
    lexseq_t *fseq = p;

    lexseq_free(expr_lexmemctx(vctx), fseq);

} /* field_free */

/*
 * field_copy
 *
 * Copy-constructor for a field.
 */
static int
field_copy (void *vctx, name_t *dst, void *dp,
                name_t *src, void *sp)
{
    lexseq_t *dseq = dp;
    lexseq_t *sseq = sp;

    return lexseq_copy(expr_lexmemctx(vctx), dseq, sseq);

} /* field_copy */

/*
 * field_lexseq
 *
 * Getter for the lexeme-sequence that constitutes a field.
 */
lexseq_t *field_lexseq (name_t *fnp) { return name_extraspace(fnp); }


/*
 * field_extract
 *
 * Extracts the 'n'th parameter from a field.
 * Used for the implementation of %FIELDEXPAND.
 */
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

} /* field_extract */

/*
 * fieldset_init
 *
 * Field-set constructor.
 */
static int
fieldset_init (void *vctx, name_t *np, void *p)
{
    namereflist_t *refs = p;

    namereflist_init(refs);

    return 1;

} /* fieldset_init */

/*
 * fieldset_free
 *
 * Field-set destructor.
 */
static void
fieldset_free (void *vctx, name_t *np, void *p)
{
    expr_ctx_t ctx = vctx;
    namectx_t namectx = expr_namectx(ctx);
    namereflist_t *refs = p;

    namereflist_free(namectx, refs);

} /* fieldset_free */

/*
 * fieldset_copy
 *
 * Copy-constructor for field sets.  In the copy,
 * the nameref's must point to the entries in the
 * newly-constructed scope.
 */
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

/*
 * fieldset_reflist
 *
 * Getter for the namereflist that constitutes a field set.
 */
namereflist_t *fieldset_reflist (name_t *fsnp) { return name_extraspace(fsnp); }

/*
 * predeclare_structures
 *
 * Callback routine that fetches the text of the structure
 * declarations set up at initialization time.
 */
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

} /* predeclare_structures */

/*
 * structures_init
 *
 * Module initialization.  Sets up the expression-binding
 * routine, the name table management hooks, and the
 * predeclared structures.
 */
void
structures_init (expr_ctx_t ctx, scopectx_t kwdscope)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    namectx_t namectx = expr_namectx(ctx);
    struct pdclinfo_s pdinfo;
    machinedef_t *mach = expr_machinedef(ctx);
    int nounits = (machine_scalar_units(mach) == 1);
    int signext = machine_signext_supported(mach);
    nametype_vectors_t vec;
    int i;

    for (i = 0; i < sizeof(mynames)/sizeof(mynames[0]); i++) {
        name_declare(kwdscope, &mynames[i], 0, 0, 0, 0);
    }
    expr_dispatch_register(ctx, LEXTYPE_NAME_STRUCTURE, structure_bind);
    parser_lexfunc_register(pctx, ctx, LEXTYPE_LXF_FIELDEXPAND, parse_FIELDEXPAND);
    parser_lexfunc_register(pctx, ctx, LEXTYPE_LXF_SIZE, parse_SIZE);

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
 *
 * Parses a STRUCTURE declaration.
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
    static lextype_t delims[] = { LEXTYPE_DELIM_RBRACK, LEXTYPE_DELIM_SEMI };

    while (term == LEXTYPE_DELIM_COMMA) {
        if (!parse_decl_name(pctx, &struname, &pos)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, "[");
        }
        memset(&ndef, 0, sizeof(ndef));
        ndef.lt = LEXTYPE_NAME_STRUCTURE;
        ndef.flags = NAME_M_DECLARED;
        ndef.name = struname->ptr;
        ndef.namelen = struname->len;
        np = name_declare(scope, &ndef, pos, 0, 0, &stru);
        if (np == 0) {
            expr_signal(ctx, STC__INTCMPERR, "declare_structure");
        }
        which = macro_paramlist(ctx, 0, 1, 0, delims, 2,
                                &stru->acctbl, &stru->accformals);
        if (which < 0) {
            expr_signal(ctx, STC__SYNTAXERR);
        } else if (which == 1) {
            which = macro_paramlist(ctx, 0, 1, 0, delims, 1,
                                    &stru->allotbl, &stru->alloformals);
            if (which < 0) {
                expr_signal(ctx, STC__SYNTAXERR);
            }
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            expr_signal(ctx, STC__OPEREXP, "=");
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            parser_scope_begin(pctx);
            if (!parse_lexeme_seq(pctx, 0, QL_MACRO, delims, 1, &stru->allobody, 0)) {
                expr_signal(ctx, STC__SYNTAXERR);
            }
            parser_scope_end(pctx);
        }
        parser_scope_begin(pctx);
        if (!parse_lexeme_seq(pctx, 0, QL_MACRO, bodyends, 2, &stru->accbody, &term)) {
            expr_signal(ctx, STC__SYNTAXERR);
        }
        parser_scope_end(pctx);

    } /* while */

    return 1;

} /* declare_structure */

/*
 * parse_fields
 *
 * Recursive routine for defining individual fields
 * and field-sets.  If the 'fldset' parameter is null,
 * we check for the SET keyword and call ourselves again
 * to parse the fields in the set, if that keyword is
 * encountered.
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
        if (!parse_decl_name(pctx, &fldname, &fpos)) {
            expr_signal(ctx, STC__NAMEEXP);
            break;
        }
        memset(&ndef, 0, sizeof(ndef));
        ndef.name = fldname->ptr;
        ndef.namelen = fldname->len;
        ndef.flags = NAME_M_DECLARED;
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_OP_ASSIGN, 0, 1)) {
            expr_signal(ctx, STC__OPEREXP, "=");
        }
        // Check for fieldset and recurse
        if (fldset == 0 && parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
            namereflist_t *frefs;
            ndef.lt = LEXTYPE_NAME_FIELDSET;
            fldnp = name_declare(scope, &ndef, fpos, 0, 0, &frefs);
            if (fldnp == 0) {
                expr_signal(ctx, STC__INTCMPERR, "parse_fields[1]");
                break;
            }
            if (!parse_fields(ctx, scope, frefs)) {
                expr_signal(ctx, STC__SYNTAXERR);
                return 0;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
                expr_signal(ctx, STC__KWDEXP, "TES");
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
            expr_signal(ctx, STC__INTCMPERR, "parse_fields[2]");
            break;
        }
        if (fldset != 0) {
            namereflist_instail(fldset, nameref_alloc(namectx, fldnp));
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, "]");
        }
        while (1) {
            if (expr_parse_ctce(ctx, &lex, 0)) {
                lexseq_instail(fseq, lex);
            } else {
                expr_signal(ctx, STC__EXPCTCE);
                lexseq_instail(fseq,
                               lexeme_create(parser_lexmemctx(pctx),
                                             LEXTYPE_NUMERIC, &zero));
            }
            which = parser_expect_oneof(pctx, QL_NORMAL, delims, 2, &lex, 1);
            if (which != 0) {
                if (which < 0) {
                    expr_signal(ctx, STC__DELIMEXP, ",");
                }
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
 *
 * Entry point for parsing FIELD declarations.
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
 *
 * Expands a structure allocation.
 */
int
structure_allocate (expr_ctx_t ctx, name_t *struname,
                    strudef_t **strup, unsigned int *nunits,
                    scopectx_t *scopep, int is_ref)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    strudef_t *stru = name_extraspace(struname);
    machinedef_t *mach = expr_machinedef(ctx);
    lexctx_t lctx = expr_lexmemctx(ctx);
    lexseq_t tmpseq;
    scopectx_t myscope, retscope;
    nameref_t *ref;
    int nomoreactuals;
    int nobrackets = 0;
    int allowed_aus = 0;
    static lextype_t aus[4] = { LEXTYPE_AU_BYTE, LEXTYPE_AU_WORD,
        LEXTYPE_AU_LONG, LEXTYPE_AU_QUAD };
    static lextype_t su[2] = { LEXTYPE_ATTR_UNSIGNED, LEXTYPE_ATTR_SIGNED };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
        nobrackets = 1;
    }

    // Set up for matching the allocation-unit names to byte counts,
    // but only on byte-addressable machines
    if (machine_unit_bits(mach) == 8) {
        for (allowed_aus = 0; allowed_aus < 4 &&
             machine_scalar_units(mach) >= (1<<allowed_aus); allowed_aus++);
    }

    myscope = parser_scope_begin(pctx);
    if (scopep != 0) {
        retscope = scope_begin(scope_namectx(myscope), 0);
    }

    // Now fill in the default values for the allocation formals, if they
    // have defaults set.
    for (ref = namereflist_head(&stru->alloformals); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0) {
            strdesc_t *alloname = name_string(ref->np);
            lexseq_t seq;
            expr_node_t *exp;
            lexseq_init(&seq);
            lexseq_copy(lctx, &seq, macparam_lexseq(ref->np));
            if (lexseq_length(&seq) > 0 && expr_parse_seq(ctx, &seq, &exp)) {
                if (expr_type(exp) == EXPTYPE_PRIM_LIT)
                    litsym_special(myscope, alloname,
                                   (unsigned int) expr_litval(exp));
                expr_node_free(ctx, exp);
            }
        }
    }
    // Now parse the allocation actuals, if any have been specified.
    // For an omitted actuals, fill in the default values, or zeros
    // where there are no explicit defaults.
    nomoreactuals = nobrackets;
    for (ref = namereflist_head(&stru->alloformals); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0) {
            name_t *np, *rnp;
            strdesc_t *alloname = name_string(ref->np);
            unsigned long val;
            int i = -1;
            np = 0;
            // An actual could be one of the allocation-unit keywords
            // or SIGNED/UNSIGNED, on certain machines, so check for
            // those as well as for a normal compile-time constant
            // expresion.
            if (!nomoreactuals) {
                if (allowed_aus > 0) {
                    i = parser_expect_oneof(pctx, QL_NORMAL, aus,
                                            allowed_aus, 0, 1);
                    if (i >= 0) {
                        val = 1L << i;
                    }
                }
                if ((i < 0) && machine_signext_supported(mach)) {
                    i = parser_expect_oneof(pctx, QL_NORMAL, su, 2, 0, 1);
                    if (i >= 0) {
                        val = i;
                    }
                }
                if ((i < 0) && expr_parse_ctce(ctx, 0, (long *)&val)) {
                    np = litsym_special(myscope, alloname, val);
                    i = 0;
                }
            }
            if (i < 0) {
                np = litsym_search(myscope, alloname, &val);
                if (np == 0) {
                    val = 0;
                }
            }
            if (np == 0) {
                np = litsym_special(myscope, alloname, val);
                if (np == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "structure_allocate[4]");
                }
            }
            // Now copy the declaration into the scope we'll pass back
            // to the caller for later use
            if (scopep != 0) {
                rnp = litsym_special(retscope, alloname, val);
                if (rnp == 0) {
                    expr_signal(ctx, STC__INTCMPERR, "structure_allocate[5]");
                }
            }
        }
        if (!nomoreactuals &&
            !parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            nomoreactuals = 1;
        }
    } /* loop through all of the allocation parameters */

    if (!nobrackets &&
        !parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "]");
    }

    // A structure definition may not actually have an
    // allocation part, but may only be used for accessing
    // storage allocated in some other way.
    if (lexseq_length(&stru->allobody) == 0) {
        *nunits = 0;
    } else {
        long val;
        lexseq_init(&tmpseq);
        lexseq_copy(lctx, &tmpseq, &stru->allobody);
        parser_insert_seq(pctx, &tmpseq);
        if (!expr_parse_ctce(ctx, 0, &val)) {
            expr_signal(ctx, STC__EXPCTCE);
            return 0;
        }
        *nunits = (unsigned int) val;
    }
    if (scopep != 0) *scopep = retscope;
    parser_scope_end(pctx);
    if (strup != 0) *strup = stru;

    return 1;

} /* structure_allocate */

/*
 * structure_reference
 *
 * Expands a structure reference, for both the general case
 * and the ordinary case.
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
    textpos_t pos = parser_curpos(pctx);
    strudef_t *stru = name_extraspace(struname);
    static lextype_t delims[3] = { LEXTYPE_DELIM_RBRACK, LEXTYPE_DELIM_COMMA,
        LEXTYPE_DELIM_SEMI };
    int ndelims;

    fldscope = 0;
    lexseq_init(&seq);
    // If this is a general structure reference, get the
    // address expression
    if (symname == 0) {

		// It might be more correct to use expr_parse_expr() here to get
		// the address expression, rather than deferring that to the
		// expansion step later, but just handling it lexically was
		// simpler to implement. XXX
        ndelims = 3;
        if (!parse_lexeme_seq(pctx, 0, QL_NORMAL, delims, 3, &seq, &delim)) {
            return 0;
        }
        myscope = scope_copy(stru->acctbl, 0);
    } else {
    	// Here we have an ordinary structure reference, and can use
    	// the symbol name for the base address.
        data_attr_t *attr = datasym_attr(symname);
        if (attr == 0) {
            expr_signal(ctx, STC__INTCMPERR, "structure_reference[1]");
            return 0;
        }
        lexseq_instail(&seq,lexeme_copy(lctx, curlex));
        // Semicolons (and allocation-formals) not allowed in this case
        ndelims = 2;
        delim = LEXTYPE_DELIM_COMMA;
        myscope = ((attr->struscope == 0)
                   ? scope_begin(expr_namectx(ctx), 0)
                   : scope_copy(attr->struscope, 0));
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
	// In the expansion, the name of the structure represents the
	// base address, so define that as a macro parameter.
    macparam_special(myscope, name_string(struname), &seq);
    lexseq_free(lctx, &seq);

	// Now parse the access-actuals, bringing the field names into scope.
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
                    expr_signal(ctx, STC__SYNTAXERR);
                    break;
                }
                if (ctce_accessors) {
                    lexseq_t testseq;
                    lexseq_init(&testseq);
                    lexseq_copy(lctx, &testseq, &seq);
                    if (!expr_parse_seq(ctx, &testseq, &exp) ||
                        expr_type(exp) != EXPTYPE_PRIM_LIT) {
                        expr_signal(ctx, STC__EXPCTCE);
                    } else {
                        expr_node_free(ctx, exp);
                    }
                    lexseq_free(lctx, &testseq);
                }
            }
            if (lexseq_length(&seq) == 0) {
                macparam_lookup(lctx, stru->acctbl, pname, &seq);
            }
            // Parenthesize the parameter value if it's non-null, because we're
            // doing lexical substitution and this could be used in an expression
            // that is expecting it to be a single operand.
            if (lexseq_length(&seq) > 0) {
                lexseq_inshead(&seq, lexeme_create(lctx, LEXTYPE_DELIM_LPAR, &leftparen));
                lexseq_instail(&seq, lexeme_create(lctx, LEXTYPE_DELIM_RPAR, &rightparen));
            }
            macparam_special(myscope, pname, &seq);
            lexseq_free(lctx, &seq);
        }
    }
    if (fldscope != 0) {
        parser_scope_end(pctx);
    }

    // Allocation-actuals are only used in the general-reference case
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
                // Parenthesize the parameter value if it's non-null, because we're
                // doing lexical substitution and this could be used in an expression
                // that is expecting it to be a single operand.
                if (lexseq_length(&seq) > 0) {
                    lexseq_inshead(&seq, lexeme_create(lctx, LEXTYPE_DELIM_LPAR, &leftparen));
                    lexseq_instail(&seq, lexeme_create(lctx, LEXTYPE_DELIM_RPAR, &rightparen));
                }
                macparam_special(myscope, pname, &seq);
            }
        }
    }
    if (delim != LEXTYPE_DELIM_RBRACK) {
        expr_signal(ctx, STC__DELIMEXP, "]");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RBRACK);
    }

	// At this point, we have built the complete sequence
	// of lexemes to convert into the structure reference
	// expression.
    lexseq_init(&seq);
    lexseq_copy(lctx, &seq, &stru->accbody);
    parser_scope_push(pctx, myscope);
    if (!expr_parse_seq(ctx, &seq, &exp) || exp == 0) {
        expr_signal(ctx, STC__SYNTAXERR);
        lexseq_free(lctx, &seq);
        parser_scope_end(pctx);
        return 0;
    }

    parser_scope_end(pctx);

    // Since STRUREF auto-parenthesizes the expression, we can
    // simplify a resultant block expression if it only contains
    // one expression and has no declarations, labels, or codecomments.
    exp = expr_block_simplify(ctx, exp);
    if (exp == 0) {
        expr_signal(ctx, STC__INTCMPERR, "structure_reference[2]");
        return 0;
    }
    resexp = expr_node_alloc(ctx, EXPTYPE_PRIM_STRUREF, pos);
    expr_struref_accexpr_set(resexp, exp);
    expr_struref_referer_set(resexp, symname);
    expr_is_ctce_set(resexp, expr_is_ctce(exp));
    expr_is_ltce_set(resexp, expr_is_ltce_only(exp));
    expr_has_value_set(resexp, 1); // XXX *must* have value, right?
    return resexp;

} /* structure_reference */

/*
 * parse_FIELDEXPAND
 *
 * %FIELDEXPAND(field-name {,n})
 */
static int
parse_FIELDEXPAND (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    lexctx_t lctx = expr_lexmemctx(ctx);
    long val;
    lexeme_t *lex;
    int gotval = 0;
    name_t *fnp;

    if (!(parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1))) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_FIELD, &lex, 1)) {
        expr_signal(ctx, STC__FLDNAMEXP);
        return 1;
    }
    fnp = lexeme_ctx_get(lex);
    if (fnp == 0) {
        expr_signal(ctx, STC__INTCMPERR, "parse_FIELDEXPAND");
        return 1;
    }
    lexeme_free(lctx, lex);
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        gotval = 1;
        if (!expr_parse_ctce(ctx, 0, &val)) {
            expr_signal(ctx, STC__EXPCTCE);
            val = 0;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }
    if (!gotval) {
        lexseq_t tmpseq;
        lexseq_init(&tmpseq);
        lexseq_copy(lctx, &tmpseq, field_lexseq(fnp));
        parser_insert_seq(pctx, &tmpseq);
    } else {
        lex = field_extract(fnp, (unsigned int) val);
        if (lex == 0) {
            expr_signal(ctx, STC__FLDEXPERR);
            parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, &zero));
        } else {
            parser_insert(pctx, lexeme_copy(lctx, lex));
        }

    }

    return 1;

} /* parse_FIELDEXPAND */

/*
 * parse_SIZE
 *
 * %SIZE(structure-attribute)
 *
 * Returns the number of addressable units that would
 * be allocated for a data segment with the specified
 * structure attribute.
 */
static int
parse_SIZE (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt)
{
    expr_ctx_t ctx = vctx;
    lexeme_t *lex;
    strudef_t *stru;
    unsigned int units;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 0;
    }
    units = 0;
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_STRUCTURE, &lex, 1)) {
        expr_signal(ctx, STC__STRUNMEXP);
        return 1;
    }
    if (!structure_allocate(ctx, lexeme_ctx_get(lex), &stru, &units, 0, 0)) {
        expr_signal(ctx, STC__SYNTAXERR);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                                             string_printf(expr_strctx(ctx), 0,
                                                           "%u", units)));

    return 1;

} /* expr_parse_SIZE */
