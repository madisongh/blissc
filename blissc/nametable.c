//
//  nametable.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nametable.h"
#include "logging.h"
#include "strings.h"
#include "lexeme.h"

/*
 * Internal flags.
 * ALLOCATED: for distinguishing dynamically-allocated
 *            name blocks from others
 * NODCLCHK:  for overriding the normal check for
 *            redeclaration (used for macro parameter
 *            tables)
 */
#define NAME_M_ALLOCATED (1<<16)
#define NAME_M_NODCLCHK  (1<<17)
#define NAME_K_EXTRAS     4

struct name_s {
    TQ_ENT_FIELDS(struct name_s)
    lextype_t            nametype;
    scopectx_t           namescope;
    textpos_t            namedclpos;
    unsigned int         nameflags;
    size_t               namelen;
    char                 name[NAME_SIZE];
    void                *nameextra[NAME_K_EXTRAS];
};
// NB: max name length is (NAME_SIZE-1)

struct namelist_s {
    TQ_HDR_FIELDS(name_t)
};
typedef struct namelist_s namelist_t;
DEFINE_TQ_FUNCS(namelist, namelist_t, name_t)

/*
 * Hash table size.
 */
#define HT_BUCKETS  32

/*
 * Count of names at which we switch from
 * simple linear list to hash tables
 */
#define NAMECOUNT_THRESHOLD 20

/*
 * hash
 *
 * Computes the hash table index for a name.
 */
static inline int
hash (const char *str, size_t len)
{
    int result = 0;
    while (len > 0) {
        result = result ^ *str;
        str += 1;
        len -= 1;
    }
    return result & (HT_BUCKETS-1);
}

#define NAME_ALLOCOUNT  64
#define SCOPE_ALLOCOUNT 64
#define REF_ALLOCOUNT   64

/*
 * Name scope tracking structures
 */
struct hashtable_s {
    struct hashtable_s  *next;
    namereflist_t       listhead[HT_BUCKETS];
};
typedef struct hashtable_s hashtable_t;
struct namectx_s;
struct scopectx_s {
    struct scopectx_s *parent;
    struct namectx_s  *home;
    struct name_s     *sclassnames[SCLASS_COUNT];
    namelist_t         names;
    hashtable_t       *htptr;
};

/*
 * Master name table context
 */
struct namectx_s {
    logctx_t             logctx;
    struct scopectx_s   *freescopes;
    hashtable_t         *freehts;
    nameref_t           *freerefs;
    struct name_s       *freenames[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    nametype_vectors_t   typevec[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    int                  freelistindex[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    int                  nfreelists;
    struct scopectx_s   *nullscope;
    struct scopectx_s   *globalscope;
    void                *symctx;
    unsigned int         tmpcount;
};
#define roundup(s_) ((((s_)+7)>>3)<<3)
static const size_t cellsize = roundup(sizeof(name_t));
static inline int typeidx(lextype_t type) {
    if (type >= LEXTYPE_NAME_MIN && type <= LEXTYPE_NAME_MAX)
        return type-LEXTYPE_NAME_MIN;
    return 0;
}

scopectx_t name_scope (name_t *np) { return np->namescope; }
strdesc_t *name_string (name_t *np) {
    return string_from_chrs(0, np->name, np->namelen); }
lexeme_t *name_to_lexeme (lexctx_t lctx, name_t *np, textpos_t pos) {
    lexeme_t *lex;
    strdesc_t dsc;
    strdesc_init(&dsc, np->name, np->namelen);
    lex = lexeme_create(lctx, LEXTYPE_NAME, &dsc);
    if (lex != 0) lexeme_textpos_set(lex, pos);
    return lex;
}
lextype_t name_type (name_t *np) { return np->nametype; }
void *name_extraspace (name_t *np) { return np->nameextra; }
void *name_value_pointer (name_t *np) { return np->nameextra[0]; }
long name_value_signed (name_t *np) { return (long) np->nameextra[0]; }
unsigned long name_value_unsigned (name_t *np) { return (unsigned long) np->nameextra[0]; }
void name_value_pointer_set (name_t *np, void *p) { np->nameextra[0] = p; }
void name_value_signed_set (name_t *np, long v) { np->nameextra[0] = (void *) v; }
void name_value_unsigned_set (name_t *np, unsigned long v) { np->nameextra[0] = (void *) v; }
void *nametables_symctx_get (namectx_t ctx) { return ctx->symctx; }
void nametables_symctx_set (namectx_t ctx, void *ptr) { ctx->symctx = ptr; }
textpos_t name_defpos (name_t *np) { return np->namedclpos; }

/*
 * name_alloc
 *
 * Allocates and initializes a name_t structure.
 */
static name_t *
name_alloc (namectx_t ctx, lextype_t type,
            const char *name, size_t namelen)

{
    name_t *np;
    int i = ctx->freelistindex[typeidx(type)];
    size_t nsize = ctx->typevec[typeidx(type)].typesize;
    name_datainit_fn initfn = ctx->typevec[typeidx(type)].typeinit;
    void *vctx = ctx->typevec[typeidx(type)].typectx;

    if (ctx->freenames[i] == 0) {
        int j;
        ctx->freenames[i] = malloc(nsize*NAME_ALLOCOUNT);
        if (ctx->freenames[i] == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "name_alloc");
            return 0;
        }
        memset(ctx->freenames[i], 0, nsize*NAME_ALLOCOUNT);
        for (np = ctx->freenames[i], j = NAME_ALLOCOUNT; j > 1;
             np = np->tq_next, j--) {
            np->tq_next = (name_t *)((char *) np + nsize);
        }
    }
    np = ctx->freenames[i];
    ctx->freenames[i] = np->tq_next;
    memset(np, 0, nsize);
    if (namelen > NAME_SIZE-1) {
        namelen = NAME_SIZE-1;
    }
    memcpy(np->name, name, namelen);
    np->name[namelen] = '\0';
    np->nameflags = NAME_M_ALLOCATED;
    np->namelen = namelen;
    np->nametype = type;
    if (initfn != 0 && !initfn(vctx, np, np->nameextra)) {
        np->tq_next = ctx->freenames[i];
        ctx->freenames[i] = np;
        np = 0;
    }

    return np;

} /* name_alloc */

/*
 *
 * name_copy
 *
 * Copy a name_t structure.
 */
static name_t *
name_copy (name_t *src, scopectx_t dstscope)
{
    namectx_t ctx = src->namescope->home;
    name_t *dst = name_alloc(ctx, src->nametype, src->name, src->namelen);
    name_datacopy_fn cfn = ctx->typevec[typeidx(src->nametype)].typecopy;
    void *vctx = ctx->typevec[typeidx(src->nametype)].typectx;

    if (dst == 0) {
        return dst;
    }

    dst->namescope = dstscope;
    dst->nameflags = NAME_M_ALLOCATED | src->nameflags;
    dst->nametype = src->nametype;
    dst->namelen = src->namelen;
    dst->namedclpos = src->namedclpos;
    memcpy(dst->name, src->name, dst->namelen);
    if (cfn != 0) {
        cfn(vctx, dst, dst->nameextra, src, src->nameextra);
    } else {
        memcpy(dst->nameextra, src->nameextra, sizeof(dst->nameextra));
    }
    return dst;
    
} /* name_copy */

/*
 * name_free
 *
 * Frees a name_t structure, if it's owned by this module,
 * and if the name is not referenced in any scope.
 */
void
name_free (name_t *np)
{
    namectx_t ctx;
    name_datafree_fn freefn;

    if (np == 0) {
        return;
    }
    ctx = np->namescope->home;
    if (np->namescope != 0 && np->namescope != ctx->nullscope) {
        scopectx_t scope = np->namescope;
        namelist_remove(&scope->names, np);
        if (scope->htptr != 0) {
            int i = hash(np->name, np->namelen);
            nameref_t *ref;
            hashtable_t *table = scope->htptr;
            for (ref = namereflist_head(&table->listhead[i]);
                 ref != 0 && ref->np != np;
                 ref = ref->tq_next);
            if (ref != 0) {
                namereflist_remove(&table->listhead[i], ref);
            }
        }
        np->namescope = ctx->nullscope;
    }

    if (np->namescope == ctx->nullscope &&
        (np->nameflags & NAME_M_ALLOCATED)) {
        int i = typeidx(np->nametype);
        freefn = ctx->typevec[i].typefree;
        if (freefn != 0) {
            freefn(ctx->typevec[i].typectx, np, np->nameextra);
        }
        memset(np, 0xcc, ctx->typevec[i].typesize);
        np->tq_next = ctx->freenames[ctx->freelistindex[i]];
        ctx->freenames[ctx->freelistindex[i]] = np;
    }
} /* name_free */

static void
namelist_free (namectx_t ctx, namelist_t *nl)
{
    name_t *np;

    if (nl == 0) {
        return;
    }
    while ((np = namelist_remhead(nl)) != 0) {
        np->namescope = ctx->nullscope;
        name_free(np);
    }
} /* namelist_free */

static int
namelist_copy (scopectx_t dstscope, namelist_t *dst, namelist_t *src)
{
    name_t *np;

    if (dst == 0) {
        return 0;
    }
    memset(dst, 0, sizeof(namelist_t));
    if (src == 0) {
        return 0;
    }
    for (np = namelist_head(src); np != 0; np = np->tq_next) {
        name_t *npc = name_copy(np, dstscope);
        if (npc == 0) return 0;
        namelist_instail(dst, npc);
    }
    return 1;

} /* namelist_copy */

nameref_t *
nameref_alloc (namectx_t ctx, name_t *np)
{
    nameref_t *ref;
    if (ctx->freerefs == 0) {
        int i;
        ctx->freerefs = malloc(REF_ALLOCOUNT*sizeof(nameref_t));
        for (i = REF_ALLOCOUNT-1, ref = ctx->freerefs; i > 0; ref++, i--)
            ref->tq_next = ref + 1;
        ref->tq_next = 0;
    }
    if (ctx->freerefs == 0) return 0;
    ref = ctx->freerefs;
    ctx->freerefs = ref->tq_next;
    memset(ref, 0, sizeof(nameref_t));
    ref->np = np;
    return ref;
} /* nameref_alloc */

void
nameref_free (namectx_t ctx, nameref_t *ref)
{
    if (ref == 0) return;
    ref->tq_next = ctx->freerefs;
    ctx->freerefs = ref;

} /* nameref_free */

void
namereflist_free (namectx_t ctx, namereflist_t *rlist)
{
    nameref_t *ref, *rnext;
    for (ref = namereflist_head(rlist); ref != 0; ref = rnext) {
        rnext = ref->tq_next;
        nameref_free(ctx, ref);
    }

} /* namereflist_free */

int
namereflist_copy (namectx_t ctx, namereflist_t *dst, namereflist_t *src)
{
    nameref_t *s;
    for (s = namereflist_head(src); s != 0; s = s->tq_next) {
        namereflist_instail(dst, nameref_alloc(ctx, s->np));
    }
    return 1;
}

static void
destroy_hashtable (scopectx_t scope)
{
    int i;
    namectx_t ctx = scope->home;

    if (scope->htptr != 0) {
        for (i = 0; i < HT_BUCKETS; i++) {
            namereflist_free(ctx, &scope->htptr->listhead[i]);
        }
        memset(scope->htptr, 0x22, sizeof(hashtable_t));
        scope->htptr->next = scope->home->freehts;
        scope->home->freehts = scope->htptr;
        scope->htptr = 0;
    }

} /* destroy_hashtable */

static void
create_hashtable (scopectx_t scope)
{
    namectx_t ctx = scope->home;
    name_t *np;
    nameref_t *ref;
    int i;

    if (ctx->freehts == 0) {
        scope->htptr = malloc(sizeof(hashtable_t));
    } else {
        scope->htptr = ctx->freehts;
        ctx->freehts =  scope->htptr->next;
    }
    if (scope->htptr == 0) {
        return;
    }
    memset(scope->htptr, 0, sizeof(hashtable_t));
    for (np = namelist_head(&scope->names); np != 0; np = np->tq_next) {
        i = hash(np->name, np->namelen);
        ref = nameref_alloc(ctx, np);
        if (ref == 0) {
            destroy_hashtable(scope);
            return;
        }
        namereflist_instail(&scope->htptr->listhead[i], ref);
    }

} /* create_hashtable */

namectx_t
nametables_init (logctx_t logctx)
{
    namectx_t master = malloc(sizeof(struct namectx_s));

    if (master != 0) {
        int i;
        memset(master, 0, sizeof(struct namectx_s));
        master->logctx = logctx;
        master->nullscope = scope_begin(master, 0);
        master->globalscope = scope_begin(master, 0);
        // Set up the default size and freelist index
        for (i = 0; i < LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN; i++) {
            master->typevec[i].typesize = cellsize;
        }
        master->nfreelists = 1;
        master->freelistindex[0] = 0;
    }
    return master;

} /* nametables_init */

scopectx_t nametables_globalscope (namectx_t ctx) { return ctx->globalscope; }

/*
 * scope_begin
 *
 * Establishes a new name scope, underneath 'parent'.
 */
scopectx_t
scope_begin (namectx_t ctx, scopectx_t parent)
{
    scopectx_t scope;

    if (ctx->freescopes == 0) {
        int i;
        ctx->freescopes = malloc(sizeof(struct scopectx_s)*SCOPE_ALLOCOUNT);
        if (ctx->freescopes == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "scope_begin");
            return 0;
        }
        memset(ctx->freescopes, 0, sizeof(struct scopectx_s)*SCOPE_ALLOCOUNT);
        for (scope = ctx->freescopes, i = SCOPE_ALLOCOUNT; i > 1; scope++, i--)
            scope->parent = scope + 1;
    }
    scope = ctx->freescopes;
    ctx->freescopes = scope->parent;
    memset(scope, 0, sizeof(struct scopectx_s));
    scope->parent = (parent == 0 ? ctx->nullscope : parent);
    scope->home = ctx;
    if (parent != 0) {
        memcpy(scope->sclassnames, parent->sclassnames, sizeof(scope->sclassnames));
    }
    return scope;

} /* scope_begin */

/*
 * scope_end
 *
 * Deletes a name scope and all of the names in it.
 */
scopectx_t
scope_end (scopectx_t scope)
{
    namectx_t ctx;
    scopectx_t parent;

    if (scope == 0) {
        return 0;
    }
    ctx = scope->home;
    parent = scope->parent;
    namelist_free(ctx, &scope->names);
    destroy_hashtable(scope);
    memset(scope, 0x33, sizeof(struct scopectx_s));
    scope->parent = ctx->freescopes;
    ctx->freescopes = scope;
    return parent;

} /* scope_end */

/*
 * scope_copy
 *
 * Duplicates a name table.  Sets the NODCLCHK flag
 * on each of the copied names, which will allow them
 * to be redeclared, exactly once, in the new table.
 * This is for handling the instantation of macro
 * actual parameters.
 */
scopectx_t
scope_copy (scopectx_t src, scopectx_t newparent)
{

    namectx_t ctx;
    scopectx_t dst;
    name_t *dname;

    if (src == 0) {
        return 0;
    }
    ctx = src->home;
    dst = scope_begin(ctx, newparent);
    if (dst == 0) {
        return 0;
    }
    namelist_copy(dst, &dst->names, &src->names);
    if (src->htptr != 0) {
        create_hashtable(dst);
    }
    for (dname = namelist_head(&dst->names); dname != 0;
         dname = dname->tq_next) {
        dname->nameflags |= NAME_M_NODCLCHK;
    }
    memcpy(dst->sclassnames, src->sclassnames, sizeof(dst->sclassnames));
    return dst;

} /* scope_copy */

/*
 * scope_getparent
 *
 * Return the parent scope of a name table.
 */
scopectx_t
scope_getparent (scopectx_t scope) { return (scope == 0 ? 0 : scope->parent); }

/*
 * scope_setparent
 *
 * Sets the parent scope for a name table.
 */
void
scope_setparent (scopectx_t scope, scopectx_t newparent)
{
    int i;
    if (scope == 0) {
        return;
    }

    scope->parent = newparent;
    for (i = 0; i < SCLASS_COUNT; i++) {
        if (scope->sclassnames[i] == 0) {
            scope->sclassnames[i] = newparent->sclassnames[i];
        }
    }

} /* scope_setparent */

/*
 * scope_sclass_psectname
 */
name_t *
scope_sclass_psectname (scopectx_t scope, storageclass_t cl)
{
    scopectx_t nullscope = scope->home->nullscope;
    while (scope != 0 && scope != nullscope) {
        if (scope->sclassnames[cl]) {
            return scope->sclassnames[cl];
        }
        scope = scope->parent;
    }
    return 0;

} /* scope_sclass_psectname */

/*
 * scope_sclass_psectname_set
 */
void
scope_sclass_psectname_set (scopectx_t scope, storageclass_t cl, name_t *np)
{
    scope->sclassnames[cl] = np;
    
} /* scope_sclass_psectname_set */

namectx_t
scope_namectx (scopectx_t scope)
{
    return scope->home;
}

name_t *
scope_nextname (scopectx_t scope, void **ctxp)
{
    name_t *np, *ctx;
    if (ctxp == 0) {
        return 0;
    }
    ctx = *ctxp;
    if (ctx == 0) {
        *ctxp = np = namelist_head(&scope->names);
    } else {
        *ctxp = np = ctx->tq_next;
    }
    return np;

} /* scope_nextname */

/*
 * nametype_dataop_register
 *
 * Used by name table users to install extension
 * function vectors for extra data attached to names.
 */
void
nametype_dataop_register (namectx_t ctx, lextype_t lt,
                          nametype_vectors_t *vec, void *vctx)
{
    // NB: LEXTYPE_NAME_MIN == LEXTYPE_NAME, the default
    //     unbound name type, which is managed internally
    //     by this module and used for all non-declarable
    //     name types as well.
    if (lt > LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        int j, i = typeidx(lt);
        memcpy(&ctx->typevec[i], vec, sizeof(nametype_vectors_t));
        ctx->typevec[i].typectx = vctx;
        // If the type-specific data fits in the default "extra"
        // space, just use that.  Otherwise, look for a lookaside
        // list that has the same size, or create a new one if needed.
        // When calculating the desired size, we subtract off the
        // extra space already allocated by default, since we'll
        // just be reusing that.
        if (vec->typesize <= NAME_K_EXTRAS*sizeof(void *)) {
            ctx->typevec[i].typesize = cellsize;
            ctx->freelistindex[i] = 0;
        } else {
            size_t desired_size = cellsize - NAME_K_EXTRAS*sizeof(void *)
                                    + roundup(vec->typesize);
            ctx->typevec[i].typesize = desired_size;
            for (j = 0; j <= (LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN); j++) {
                if (i == j || ctx->typevec[j].typesize == 0) continue;
                if (ctx->typevec[j].typesize == desired_size) break;
            }
            if (j > LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN) {
                ctx->freelistindex[i] = ctx->nfreelists;
                ctx->nfreelists += 1;
            } else {
                ctx->freelistindex[i] = ctx->freelistindex[j];
            }
        }
    }

} /* nametype_dataop_register */

/*
 * name_search
 *
 * Looks up a name, starting from the specified scope and working
 * backwards through the ancestor scopes.  Returns the found name_t
 * structure, if the name was found and is declared as something
 * (i.e, has a nametype other than LEXTYPE_NAME).
 */
static name_t *
name_search_internal (scopectx_t curscope, const char *id, size_t len,
                      lextype_t *ntypep, void *datapp, int undeclared_ok)
{
    int i;
    name_t *np = 0;
    nameref_t *ref = 0;
    scopectx_t nullscope = curscope->home->nullscope;
    scopectx_t scope;

    for (scope = curscope; scope != 0 && scope != nullscope; scope = scope->parent) {
        np = 0;
        if (scope->htptr != 0) {
            hashtable_t *table = scope->htptr;
            i = hash(id, len);
            for (ref = namereflist_head(&table->listhead[i]);
                 ref != 0; ref = ref->tq_next) {
                if (ref->np != 0 && len == ref->np->namelen &&
                    memcmp(id, ref->np->name, len) == 0) {
                    np = ref->np;
                    break;
                }
            }
        } else {
            for (np = namelist_head(&scope->names); np != 0; np = np->tq_next) {
                if (len == np->namelen && memcmp(id, np->name, len) == 0) {
                    break;
                }
            }
        }
        // If we're searching only for "declared" (i.e., available for use)
        // names, then make sure it's not an undeclared builtin and that
        // the name entry has a defined type (rather than the generic NAME).
        // If 'undeclared_ok' is set, we don't care about that.
        if (np != 0 && (undeclared_ok ||
                        ((np->nameflags & NAME_M_BUILTIN) == 0 &&
                         np->nametype != LEXTYPE_NAME))) {
            if (ntypep != 0) *ntypep = np->nametype;
            if (datapp != 0) *(void **)datapp = np->nameextra;
            return np;
        }
    }

    return 0;
    
} /* name_search_internal */

/*
 * External API for name search
 */
name_t *
name_search (scopectx_t curscope, const char *id, size_t len,
                     lextype_t *ntypep)
{
    return name_search_internal(curscope, id, len, ntypep, 0, 0);

} /* name_search */

name_t *
name_search_typed (scopectx_t curscope, const char *id,
                           size_t len, lextype_t ntype, void *datapp)
{
    lextype_t acttype;
    name_t *np;
    np = name_search_internal(curscope, id, len, &acttype, 0, 0);
    if (acttype != ntype) return 0;
    if (datapp != 0) *(void **)datapp = np->nameextra;
    return np;

} /* name_search_typed */

int
name_is_declared (scopectx_t curscope, const char *id, size_t len)
{
    lextype_t acttype;
    name_t *np = name_search_internal(curscope, id, len, &acttype, 0, 0);
    return (np != 0) && ((np->nameflags & NAME_M_DECLARED) != 0);
}
/*
 * name_insert
 *
 * Inserts the name_t structure into the hash table.
 */
void
name_insert (scopectx_t scope, name_t *np)
{
    int i;
    nameref_t *ref;

    if (np == 0 || scope == 0) {
        return;
    }
    np->namescope = scope;
    namelist_instail(&scope->names, np);
    if (scope->htptr == 0) {
        if (namelist_length(&scope->names) > NAMECOUNT_THRESHOLD) {
            create_hashtable(scope);
        }
        return;
    }
    ref = nameref_alloc(scope->home, np);
    if (ref == 0) {
        log_signal(scope->home->logctx, 0, STC__OUTOFMEM, "name_insert");
        destroy_hashtable(scope);
        return;
    }

    i = hash(np->name, np->namelen);
    namereflist_instail(&scope->htptr->listhead[i], ref);

} /* name_insert */

/*
 * name_declare
 *
 * Adds a name to a name table, with the specified type and data.
 */
static name_t *
name_declare_internal (scopectx_t scope, const char *id, size_t len,
                       lextype_t type, unsigned int flags,
                       textpos_t pos, void *datap,
                       size_t datasize, void *datapp, int rescheck)
{
    namectx_t namectx = scope_namectx(scope);
    name_t *np;
    lextype_t nt;

    if (len >= NAME_SIZE) {
        len = NAME_SIZE-1;
    }

    np = name_search_internal(scope, id, len, &nt, 0, 1);
    if (!rescheck && np != 0) {
        // with the override, we go ahead and define
        // the name in the local scope, regardless
        // of outer definitions
        if (np->namescope != scope) {
            np = 0;
        }
    } else if (np != 0) {
        if (np->namescope == scope && nt != LEXTYPE_NAME &&
            !(np->nameflags & NAME_M_NODCLCHK)) {
            // Redeclaration of FORWARDs is allowed, but only if
            // the type matches
            if (!(np->nameflags & NAME_M_FORWARD) || nt != type) {
                log_signal(namectx->logctx, pos, STC__REDECLARE, id, len);
                return 0;
            }
        }
        if ((np->nameflags & NAME_M_RESERVED) != 0) {
            log_signal(namectx->logctx, pos, STC__RSVDECL, id, len);
            return 0;
        }
    }
    // If np is non-NULL at this point, we can overwrite
    // an existing entry.  If so, but the types don't match,
    // we need to replace the existing entry (and hope that
    // nobody has a dangling pointer to it!).
    if (np != 0 && nt != type) {
        name_free(np);
        np = 0;
    }
    if (np == 0) {
        np = name_alloc(scope->home, type, id, len);
        if (np == 0) {
            return 0;
        }
        name_insert(scope, np);
    }

    np->nametype = type;
    np->nameflags &= ~NAME_M_NODCLCHK;
    np->nameflags &= ~NAME_M_FLAGMASK;
    np->nameflags |= (NAME_M_FLAGMASK & flags);
    np->namedclpos = pos;
    if (datap != 0) {
        name_datacopy_fn cfn = namectx->typevec[typeidx(type)].typecopy;
        if (cfn != 0) {
            cfn(namectx->typevec[typeidx(type)].typectx,
                np, np->nameextra, 0, datap);
        } else {
            if (datasize > namectx->typevec[typeidx(type)].typesize) {
                datasize = namectx->typevec[typeidx(type)].typesize;
            }
            memcpy(np->nameextra, datap, datasize);
        }
    }
    if (datapp != 0) *(void **)datapp = np->nameextra;

    return np;

} /* name_declare_internal */

name_t *
name_declare (scopectx_t scope, namedef_t *def,
              textpos_t pos, void *datap, size_t datasize, void *datapp) {
    return name_declare_internal(scope,
                                 def->name, def->namelen,
                                 def->lt, def->flags, pos,
                                 datap, datasize, datapp, 1);
} /* name_declare */

name_t *
name_declare_nocheck (scopectx_t scope, namedef_t *def,
                      textpos_t pos, void *datap, size_t datasize, void *datapp) {
    return name_declare_internal(scope,
                                 def->name, def->namelen,
                                 def->lt, def->flags, pos,
                                 datap, datasize, datapp, 0);
} /* name_declare_nocheck */
/*
 * name_undeclare
 *
 * Create a LEXTYPE_NAME entry in the scope table to represent
 * an undeclared name.  If the name being undeclared is present
 * in that scope, free it first.
 *
 * The new entry is created to prevent the search algorithm
 * from finding the name in an ancestral scope.
 */
int
name_undeclare (scopectx_t scope, name_t *np)
{
    namectx_t ctx = scope->home;
    name_t *undeclarednp;

    if (np->nameflags & NAME_M_RESERVED) {
        return 0;
    }
    undeclarednp = name_alloc(ctx, LEXTYPE_NAME, np->name, np->namelen);

    if (np->namescope == scope) {
        name_free(np);
    }
    if (undeclarednp != 0) {
        name_insert(scope, undeclarednp);
    }
    return 1;

} /* name_undeclare */

strdesc_t *
tempname_get (namectx_t ctx)
{
    ctx->tmpcount += 1;
    if (ctx->tmpcount > 999999) {
        log_signal(ctx->logctx, 0, STC__EXCTNCNT);
    }
    return string_printf(0, "%%TMP$%06u", ctx->tmpcount);
}

int
name_declare_builtin (scopectx_t scope, strdesc_t *namestr, textpos_t pos)
{
    name_t *np = name_search(scope, namestr->ptr, namestr->len, 0);

    if (np == 0 || (np->nameflags & NAME_M_BUILTIN) == 0) {
        return 0;
    }
    np = name_copy(np, scope);
    if (np == 0) {
        logctx_t logctx = scope->home->logctx;
        log_signal(logctx, pos, STC__INTCMPERR, "name_declare_builtin");
        return 0;
    }
    np->nameflags &= ~NAME_M_BUILTIN;
    np->nameflags |= NAME_M_DECLARED;
    name_insert(scope, np);
    return 1;

}