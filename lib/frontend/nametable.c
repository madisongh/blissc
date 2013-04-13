/*
 *++
 * nametable.c - Name table management.
 *
 * This module manages name tables and lexical scoping.
 * For these purposes, a 'name' is any identifier that
 * might need to be stored (possibly with information about
 * what that identifier represents) and looked up quickly.
 * Everything in the compiler that looks like a name, including
 * keywords, is declared as a name.
 *
 * Names are always defined in a name table, called a 'scope'.
 * Scopes are chained together in a parent-child relationship,
 * representing lexical scoping rules for names.  Keywords and
 * other pre-declared name are always declared in the primary,
 * outermost scope.  Name searches always extend from the current
 * scope, through the parent scopes, until the end of the chain
 * is reached or the name is found.
 *
 * Extension data can be stored with a name; this module provides
 * some space for all names for simple value storage.  For
 * names with types in the range LEXTYPE_NAME_MIN through
 * LEXTYPE_NAME_MAX, additional extensions can be added by
 * the modules handling those name types.
 *
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "blissc/nametable.h"
#include "blissc/support/logging.h"
#include "blissc/support/fileio.h"
#include "blissc/support/strings.h"
#include "blissc/lexeme.h"

/*
 * Internal flags.
 * ALLOCATED: for distinguishing dynamically-allocated
 * name blocks from others
 * NODCLCHK:  for overriding the normal check for
 * redeclaration (used for macro parameter
 * tables)
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
    strdesc_t            namedsc;
    char                 name[NAME_SIZE];
    void                *nameextra[NAME_K_EXTRAS];
};

/*
 * Internally, names are stored in name lists in each
 * table.
 */
struct namelist_s {
    TQ_HDR_FIELDS(name_t)
};
typedef struct namelist_s namelist_t;
DEFINE_TQ_FUNCS(namelist, namelist_t, name_t)

/*
 * Many scopes are small -- e.g., parameter lists, small
 * routines that don't use many local variables.  For those,
 * we simply do a linear search through the list of names.
 *
 * For larger scopes, we construct a hash table to speed
 * up the searches.  When the number of names in a scope
 * goes over NAMECOUNT_THRESHOLD, the hash table is added
 * and maintained until the scope is ended.
 */
#define NAMECOUNT_THRESHOLD 20
#define HT_BUCKETS  32

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

// Allocation counts for expanding the lookaside lists for
// these memory pools
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
 * Master name table context.  Lookaside lists for
 * dynamically-allocated structures are maintained here.
 * For names, a separate lookaside list is maintained based
 * on the size of the cell for the type of name.  Different
 * name types with the same cell size share the same memory
 * pool.
 */
struct extenthdr_s {
    struct extenthdr_s *next;
};

struct namectx_s {
    logctx_t             logctx;
    struct scopectx_s   *freescopes;
    hashtable_t         *freehts;
    nameref_t           *freerefs;
    struct name_s       *freenames[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    nametype_vectors_t   typevec[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    void                *typectx[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    int                  freelistindex[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    int                  nfreelists;
    struct scopectx_s   *nullscope;
    struct scopectx_s   *globalscope;
    void                *symctx;
    unsigned int         tmpcount;
    struct extenthdr_s  *extents;
};

// Align name_t cells to a quadword boundary
#define roundup(s_) ((((s_)+7)>>3)<<3)
static const size_t cellsize = roundup(sizeof(name_t));

// Translate a name type into the appropriate array index for
// the various arrays listed above
static inline int typeidx(lextype_t type) {
    if (type >= LEXTYPE_NAME_MIN && type <= LEXTYPE_NAME_MAX)
        return type-LEXTYPE_NAME_MIN;
    return 0;
}

/*
 * Getter/setter functions for names.
 */
scopectx_t name_scope (name_t *np) { return np->namescope; }
strdesc_t *name_string (name_t *np) { return &np->namedsc; }
char *name_azstring (name_t *np) { return np->name; }
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
unsigned int name_flags(name_t *np) { return np->nameflags; }


/*
 * name_to_lexeme
 *
 * Creates an unbound lexeme for a name.
 */
lexeme_t *
name_to_lexeme (lexctx_t lctx, name_t *np)
{
    lexeme_t *lex;
    lex = lexeme_create(lctx, LEXTYPE_NAME, &np->namedsc);
    return lex;

} /* name_to_lexeme */

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
    void *vctx = ctx->typectx[typeidx(type)];

    if (ctx->freenames[i] == 0) {
        int j;
        struct extenthdr_s *extent = malloc(sizeof(struct extenthdr_s) + nsize*NAME_ALLOCOUNT);
        if (extent == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "name_alloc");
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freenames[i] = (name_t *)(extent + 1);
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
    strdesc_init(&np->namedsc, np->name, namelen);
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
    name_t *dst = name_alloc(ctx, src->nametype, src->name, src->namedsc.len);
    name_datacopy_fn cfn = ctx->typevec[typeidx(src->nametype)].typecopy;
    void *vctx = ctx->typectx[typeidx(src->nametype)];

    if (dst == 0) {
        return dst;
    }

    dst->namescope = dstscope;
    dst->nameflags = NAME_M_ALLOCATED | src->nameflags;
    dst->nametype = src->nametype;
    strdesc_init(&dst->namedsc, dst->name, src->namedsc.len);
    dst->namedclpos = src->namedclpos;
    memcpy(dst->name, src->name, dst->namedsc.len);
    if (cfn != 0) {
        cfn(vctx, dst, dst->nameextra, src, src->nameextra);
    } else {
        memcpy(dst->nameextra, src->nameextra,
               ctx->typevec[typeidx(src->nametype)].typesize);
    }
    return dst;

} /* name_copy */

/*
 * name_free
 *
 * Frees a name_t structure.
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

    // Remove the name from its scope, if it has been
    // inserted in one
    if (np->namescope != 0 && np->namescope != ctx->nullscope) {
        scopectx_t scope = np->namescope;
        namelist_remove(&scope->names, np);
        if (scope->htptr != 0) {
            int i = hash(np->name, np->namedsc.len);
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

    // OK to free it if it has been dynamically allocated
    // and doesn't belong to any scope.
    if (np->namescope == ctx->nullscope &&
        (np->nameflags & NAME_M_ALLOCATED)) {
        int i = typeidx(np->nametype);
        freefn = ctx->typevec[i].typefree;
        if (freefn != 0) {
            freefn(ctx->typectx[i], np, np->nameextra);
        }
        memset(np, 0xcc, ctx->typevec[i].typesize);
        np->tq_next = ctx->freenames[ctx->freelistindex[i]];
        ctx->freenames[ctx->freelistindex[i]] = np;
    }
} /* name_free */

/*
 * namelist_free
 *
 * Frees all of the names in the list.
 */
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

/*
 * namelist_copy
 *
 * Creates a copy of each name in a list, putting the copies
 * into the destination list.
 */
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

/*
 * nameref_alloc
 *
 * Allocates a namref structure.
 */
nameref_t *
nameref_alloc (namectx_t ctx, name_t *np)
{
    nameref_t *ref;
    if (ctx->freerefs == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s) + REF_ALLOCOUNT*sizeof(nameref_t));
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freerefs = (nameref_t *)(extent + 1);
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

/*
 * nameref_free
 *
 * Frees a nameref.
 */
void
nameref_free (namectx_t ctx, nameref_t *ref)
{
    if (ref == 0) return;
    ref->tq_next = ctx->freerefs;
    ctx->freerefs = ref;

} /* nameref_free */

/*
 * namereflist_free
 *
 * Frees all of the namerefs on the specified list.
 */
void
namereflist_free (namectx_t ctx, namereflist_t *rlist)
{
    nameref_t *ref, *rnext;
    for (ref = namereflist_head(rlist); ref != 0; ref = rnext) {
        rnext = ref->tq_next;
        nameref_free(ctx, ref);
    }

} /* namereflist_free */

/*
 * namereflist_copy
 *
 * Creates a copy of a namreflist.
 */
int
namereflist_copy (namectx_t ctx, namereflist_t *dst, namereflist_t *src)
{
    nameref_t *s;
    for (s = namereflist_head(src); s != 0; s = s->tq_next) {
        namereflist_instail(dst, nameref_alloc(ctx, s->np));
    }
    return 1;

} /* namereflist_copy */

/*
 * destroy_hashtable
 *
 * Frees up a hash table.  Called when ending a scope, if
 * the scope has been hashified.
 */
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

/*
 * create_hashtable
 *
 * Allocate a hashtable and create entries for any names
 * already existing in the scope.
 */
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
        i = hash(np->name, np->namedsc.len);
        ref = nameref_alloc(ctx, np);
        if (ref == 0) {
            destroy_hashtable(scope);
            return;
        }
        namereflist_instail(&scope->htptr->listhead[i], ref);
    }

} /* create_hashtable */

/*
 * nametables_init
 *
 * Module initialization, setting up the master context.
 */
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

/*
 * nametables_finish
 *
 * Module shutdown.
 */
void
nametables_finish (namectx_t ctx)
{
    hashtable_t *ht, *htnext;
    struct extenthdr_s *e, *enext;
    for (ht = ctx->freehts; ht != 0; ht = htnext) {
        htnext = ht->next;
        free(ht);
    }
    for (e = ctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }
    free(ctx);
    
} /* nametables_finish */

/*
 * Setters/getters for the namectx structure
 */
scopectx_t nametables_globalscope (namectx_t ctx) { return ctx->globalscope; }

/*
 * scope_begin
 *
 * Establishes a new name scope, underneath 'parent'.  If 'parent' is
 * zero, the parent is set to a special 'null' scope, to assist in
 * locating the module master context when needed.
 */
scopectx_t
scope_begin (namectx_t ctx, scopectx_t parent)
{
    scopectx_t scope;

    if (ctx->freescopes == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s)+sizeof(struct scopectx_s)*SCOPE_ALLOCOUNT);
        if (extent == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "scope_begin");
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freescopes = (scopectx_t)(extent + 1);
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
 * Deletes a name scope and all of the names in it.  Returns
 * the parent scope, if any.
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
    return (parent == ctx->nullscope ? 0 : parent);

} /* scope_end */

/*
 * scope_copy
 *
 * Duplicates a scope.  Sets the NODCLCHK flag
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
 * Getters/setters for scopes
 */
scopectx_t scope_getparent (scopectx_t scope) {
    return (scope == 0 ? 0 : scope->parent); }
void scope_setparent (scopectx_t scope, scopectx_t newparent) {
    if (scope != 0) scope->parent = newparent; }
void scope_sclass_psectname_set (scopectx_t scope, storageclass_t cl, name_t *np) {
    scope->sclassnames[cl] = np; }
namectx_t scope_namectx (scopectx_t scope) { return scope->home; }


/*
 * scope_sclass_psectname
 *
 * Scan the current scope and up, looking for a psect name for
 * the given class.
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
 * scope_nextname
 *
 * A rather primitive walker through a scope.  Callers
 * must pass a pointer to a void pointer that has been
 * initialized to zero on the first call; that void pointer
 * is used as the walk context and should not be modified
 * by the caller until this routine returns NULL to indicate
 * the end of the list.
 */
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
 * scope_serialize
 *
 * Walks through a scope and serializes declared names.
 * Used to generate library files.
 */
int
scope_serialize (scopectx_t scope, void *fh)
{
    uint16_t buf[2];
    int status = 1;

    if (scope != 0) {
        namectx_t ctx;
        name_t *np;
        ctx = scope_namectx(scope);
        for (np = namelist_head(&scope->names); np != 0; np = np->tq_next) {
            int i;
            name_serialize_fn serfn;
            if ((np->nameflags & (NAME_M_DECLARED|NAME_M_FROMLIB)) != NAME_M_DECLARED) {
                continue;
            }
            i = typeidx(np->nametype);
            serfn = ctx->typevec[i].typeser;
            if (serfn != 0) {
                if (!serfn(ctx->typectx[i], np, fh)) {
                    status = 0;
                    break;
                }
            }
        }
    }

    // Write the end-of-scope marker

    if (status) {
        buf[0] = buf[1] = 0xFFFF;
        if (file_writebuf(fh, buf, sizeof(buf)) < 0) {
            status = 0;
        }
    }
    return status;

} /* scope_serialize */

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
        ctx->typectx[i] = vctx;
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
 * name_search_internal
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
                if (ref->np != 0 && len == ref->np->namedsc.len &&
                    memcmp(id, ref->np->name, len) == 0) {
                    np = ref->np;
                    break;
                }
            }
        } else {
            for (np = namelist_head(&scope->names); np != 0; np = np->tq_next) {
                if (len == np->namedsc.len && memcmp(id, np->name, len) == 0) {
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
 * name_search
 *
 * The public API for general name lookups.
 */
name_t *
name_search (scopectx_t curscope, const char *id, size_t len,
                     lextype_t *ntypep)
{
    return name_search_internal(curscope, id, len, ntypep, 0, 0);

} /* name_search */

/*
 * name_search_typed
 *
 * Performs a search for a name, checks that its type matches
 * the desired type, and optionally returns the pointer to
 * the extension area for the name.
 */
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

/*
 * name_search_typed
 *
 * Same as name_search_typed, but lets through undeclared
 * names.
 */
name_t *
name_search_typed_special (scopectx_t curscope, const char *id,
                           size_t len, lextype_t ntype, void *datapp)
{
    lextype_t acttype;
    name_t *np;
    np = name_search_internal(curscope, id, len, &acttype, 0, 1);
    if (acttype != ntype) return 0;
    if (datapp != 0) *(void **)datapp = np->nameextra;
    return np;

} /* name_search_typed_special */

/*
 * name_is_declared
 *
 * Checks to see if the specified name has been explicitly declared.
 */
int
name_is_declared (scopectx_t curscope, const char *id, size_t len)
{
    lextype_t acttype;
    name_t *np = name_search_internal(curscope, id, len, &acttype, 0, 0);
    return (np != 0) && ((np->nameflags & NAME_M_DECLARED) != 0);

} /* name_is_declared */

/*
 * sym_globalname
 *
 * Returns the name_t pointer in the global symbol
 * table for global/external symbols.
 */
name_t *
name_globalname (namectx_t namectx, name_t *np)
{
    strdesc_t *ndsc = name_string(np);

    return name_search_typed(namectx->globalscope, ndsc->ptr, ndsc->len, name_type(np), 0);

} /* name_globalname */

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

    i = hash(np->name, np->namedsc.len);
    namereflist_instail(&scope->htptr->listhead[i], ref);

} /* name_insert */

/*
 * name_declare_internal
 *
 * Adds a name to a name table, with the specified type and data.
 * This is the internal version, with the full range of options.
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
            // Signal REDECLARE only if the type doesn't match;
            // let the caller handle other redeclaration conditions XXX
            if (nt != type) {
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
        if (np->namescope == scope) {
            name_free(np);
        }
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
            cfn(namectx->typectx[typeidx(type)],
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

/*
 * name_declare
 *
 * Public API for declaring names, with checks for reserved words
 * and redeclaration of names already defined in ancestor scopes.
 */
name_t *
name_declare (scopectx_t scope, namedef_t *def,
              textpos_t pos, void *datap, size_t datasize, void *datapp) {
    return name_declare_internal(scope,
                                 def->name, def->namelen,
                                 def->lt, def->flags, pos,
                                 datap, datasize, datapp, 1);
} /* name_declare */

/*
 * name_declare_nocheck
 *
 * Public API for declaring names, bypassing the normal checks.  Should
 * only be used in special cases.
 */
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
name_undeclare (scopectx_t scope, name_t *np, textpos_t pos)
{
    namectx_t ctx = scope->home;
    name_t *undeclarednp;

    if (np->nameflags & NAME_M_RESERVED) {
        log_signal(scope->home->logctx, pos, STC__UNDECRSVD,
                   np->name, np->namedsc.len);
        return 0;
    }
    undeclarednp = name_alloc(ctx, LEXTYPE_NAME, np->name, np->namedsc.len);

    if (np->namescope == scope) {
        name_free(np);
    }
    if (undeclarednp != 0) {
        name_insert(scope, undeclarednp);
    }
    return 1;

} /* name_undeclare */

/*
 * tempname_get
 *
 * Creates a "temp" name that can be used to declare a name
 * that should not conflict with any user-declared name (or
 * previously used temp name).
 * Used for PLITs and other typically unnamed data that gets allocated,
 * since every data segment must be referenced through some name.
 */
int
tempname_get (namectx_t ctx, char *buf, size_t bufsiz)
{
    ctx->tmpcount += 1;
    if (ctx->tmpcount > 999999) {
        log_signal(ctx->logctx, 0, STC__EXCTNCNT);
    }
    return snprintf(buf, bufsiz, "%%TMP$%06u", ctx->tmpcount);

} /* tempname_get */

/*
 * name_declare_builtin
 *
 * Called from BUILTIN declaration processing, this routine activates
 * a name that has been registered as a builtin, so it can be used
 * in the current scope.
 */
int
name_declare_builtin (scopectx_t scope, strdesc_t *namestr, textpos_t pos)
{
    name_t *np = name_search_internal(scope, namestr->ptr, namestr->len, 0, 0, 1);

    if (np == 0 || (np->nameflags & NAME_M_BUILTIN) == 0) {
        logctx_t logctx = scope->home->logctx;
        log_signal(logctx, pos, STC__NOTBUILTN, namestr);
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

} /* name_declare_builtin */

/*
 * name_serialize
 *
 * Serializes a name_t.
 */
int
name_serialize (name_t *np, void *fh, void *extra, unsigned int extrasize)
{
    uint16_t buf[2+(NAME_SIZE/2)];
    unsigned int totlen;

    totlen = np->namedsc.len + extrasize;
    if (totlen > 65535) {
        // XXX
        return 0;
    }
    buf[0] = (uint16_t)(np->nametype);
    buf[1] = (uint16_t)totlen;
    memcpy(buf+2, np->name, np->namedsc.len);
    if (file_writebuf(fh, buf, sizeof(uint16_t)*2+np->namedsc.len) < 0) {
        return 0;
    }
    if (extrasize == 0) return 1;
    return (file_writebuf(fh, extra, extrasize) >= 0);

} /* name_serialize */

/*
 * namereflist_serialize
 *
 * Serializes a namereflist_t.  Writes out names only; no count,
 * no per-name extras.
 */
int
namereflist_serialize (namereflist_t *lst, void *fh)
{
    nameref_t *nr;
    for (nr = namereflist_head(lst); nr != 0; nr = nr->tq_next) {
        if (!name_serialize(nr->np, fh, 0, 0)) return 0;
    }
    return 1;

} /* namereflist_serialize */
