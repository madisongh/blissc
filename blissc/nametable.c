//
//  nametable.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "nametable.h"
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

/*
 * Hash table size.
 */
#define HT_BUCKETS  128

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

/*
 * Name scope tracking structures
 */
struct scopectx_s {
    struct scopectx_s *parent;
    int                namecount;
    struct name_s     *hashtable[HT_BUCKETS];
};

struct scopectx_s *freescopes = 0;
struct name_s     *freenames = 0;

static name_datafree_fn freedata[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1] = {0};
static name_datacopy_fn copydata[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1] = {0};

/*
 * name_alloc
 *
 * Allocates and initializes a name_t structure.
 */
static name_t *
name_alloc (const char *name, size_t namelen)
{
    name_t *np;
    if (freenames == 0) {
        int i = 16;
        freenames = malloc(sizeof(struct name_s)*i);
        if (freenames == 0) {
            /* XXX error condition */
            return 0;
        }
        memset(freenames, 0, sizeof(struct name_s)*i);
        for (np = freenames; i > 1; np++, i--)
            np->next = np + 1;
    }
    np = freenames;
    freenames = np->next;
    np->next = 0;
    np->namescope = 0;
    if (namelen > NAME_SIZE-1) {
        namelen = NAME_SIZE-1;
    }
    memcpy(np->name, name, namelen);
    np->name[namelen] = '\0';
    np->nameflags = NAME_M_ALLOCATED;
    np->namelen = namelen;
    np->nametype = LEXTYPE_NAME;
    memset(name_data(np), 0, NAME_DATA_SIZE);
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
    name_t *dst = name_alloc(src->name, src->namelen);
    name_datacopy_fn cfn;

    if (dst == 0) {
        return dst;
    }
    dst->nametype = src->nametype;
    dst->namescope = dstscope;
    dst->nameflags = NAME_M_ALLOCATED | src->nameflags;
    cfn = copydata[src->nametype-LEXTYPE_NAME_MIN];
    if (cfn != 0) {
        cfn(dst, src);
    } else {
        memcpy(name_data(dst), name_data(src), NAME_DATA_SIZE);
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
    name_datafree_fn freefn;

    if (np->namescope == 0 &&
        (np->nameflags & NAME_M_ALLOCATED)) {
        freefn = freedata[np->nametype-LEXTYPE_NAME_MIN];
        if (freefn != 0) {
            freefn(np);
        }
        memset(np, 0xcc, sizeof(name_t));
        np->next = freenames;
        freenames = np;
    }
} /* name_free */

/*
 * scope_begin
 *
 * Establishes a new name scope, underneath 'parent'.
 */
scopectx_t
scope_begin (scopectx_t parent)
{
    scopectx_t scope;

    if (freescopes == 0) {
        int i = 16;
        freescopes = malloc(sizeof(struct scopectx_s)*i);
        if (freescopes == 0) {
            /* XXX error condition */
            return 0;
        }
        memset(freescopes, 0, sizeof(struct scopectx_s)*i);
        for (scope = freescopes; i > 1; scope++, i--)
            scope->parent = scope + 1;
    }
    scope = freescopes;
    freescopes = scope->parent;
    memset(scope, 0, sizeof(struct scopectx_s));
    scope->parent = parent;
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
    int i;
    scopectx_t parent;

    if (scope == 0) {
        return 0;
    }
    parent = scope->parent;
    if (scope->namecount > 0) {
        for (i = 0; i < HT_BUCKETS; i++) {
            struct name_s *name, *next;
            name = scope->hashtable[i];
            while (name != 0) {
                next = name->next;
                name->namescope = 0;
                name_free(name);
                name = next;
            }
        }
    }
    scope->parent = freescopes;
    freescopes = scope;
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
    int i;
    scopectx_t dst = scope_begin(newparent);

    if (dst == 0) {
        return 0;
    }
    if (src->namecount > 0) {
        for (i = 0; i < HT_BUCKETS; i++) {
            name_t *sname, *dname, *dlast;
            sname = src->hashtable[i];
            dlast = 0;
            while (sname != 0) {
                dname = name_copy(sname, dst);
                dname->nameflags |= NAME_M_NODCLCHK;
                if (dlast == 0) {
                    dst->hashtable[i] = dlast = dname;
                } else {
                    dlast->next = dname;
                    dlast = dname;
                }
                sname = sname->next;
            }
        }
    }
    dst->namecount = src->namecount;
    return dst;

} /* scope_copy */

/*
 * scope_getparent
 *
 * Return the parent scope of a name table.
 */
scopectx_t
scope_getparent (scopectx_t scope) {

    if (scope == 0) {
        return 0;
    }
    return scope->parent;

} /* scope_getparent */

/*
 * scope_setparent
 *
 * Sets the parent scope for a name table.
 */
void
scope_setparent (scopectx_t scope, scopectx_t newparent)
{
    if (scope == 0) {
        return;
    }

    scope->parent = newparent;

} /* scope_setparent */

/*
 * nametype_datafree_register
 *
 * Used by name table users to install a
 * function in the data-freeing function table.
 */
void
nametype_dataop_register (lextype_t lt, name_datafree_fn ffn,
                          name_datacopy_fn cfn)
{
    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        freedata[lt-LEXTYPE_NAME_MIN] = ffn;
        copydata[lt-LEXTYPE_NAME_MIN] = cfn;
    }
} /* nametype_datafree_register */

/*
 * name_search
 *
 * Looks up a name, starting from the specified scope and working
 * backwards through the ancestor scopes.  Returns the found name_t
 * structure, if the name was found and not marked UNDECLAREd.
 * If the name is not found, and 'do_create' is set, the name
 * is inserted in the current scope, marked UNDECLAREd.
 */
name_t *
name_search (scopectx_t curscope, const char *id, size_t len, int do_create)
{
    int i;
    name_t *np = 0;
    scopectx_t scope;

    for (scope = curscope; scope != 0; scope = scope->parent) {
        if (scope->namecount > 0) {
            i = hash(id, len);
            for (np = scope->hashtable[i]; np != 0;
                 np = np->next) {
                if (np->nametype != LEXTYPE_NAME &&
                    len == np->namelen &&
                    memcmp(id, np->name, len) == 0) {
                    return np;
                }
            }
        }
    }

    if (do_create) {
        np = name_alloc(id, len); // automatically sets as undeclared
        name_insert(curscope, np);
    }
    return np;
    
} /* name_search */

/*
 * name_insert
 *
 * Inserts the name_t structure into the hash table.
 */
void
name_insert (scopectx_t scope, name_t *np)
{
    int i;
    i = hash(np->name, np->namelen);
    np->next = scope->hashtable[i];
    scope->hashtable[i] = np;

    scope->namecount += 1;
    np->namescope = scope;

} /* name_insert */

/*
 * name_declare
 *
 * Adds a name to a name table, with the specified type and data.
 */
name_t *
name_declare (scopectx_t scope, const char *id, size_t len,
              lextype_t type, textpos_t pos, void *value, size_t valsize)
{
    name_t *np;

    if (len >= NAME_SIZE) {
        len = NAME_SIZE-1;
    }

    np = name_search(scope, id, len, 0);
    if (np != 0) {
        if (np->namescope == scope && np->nametype != LEXTYPE_NAME &&
            !(np->nameflags & NAME_M_NODCLCHK)) {
            /* XXX error condition - redeclaration */
            return 0;
        }
        if (np->nameflags & NAME_M_RESERVED) {
            /* XXX error condition - reserved word */
            return 0;
        }
    } else {
        np = name_alloc(id, len);
        name_insert(scope, np);
    }

    np->nametype = type;
    np->nameflags &= ~NAME_M_NODCLCHK;
    np->nameflags |= NAME_M_DECLARED;
    np->namedclpos = pos;
    if (valsize > NAME_DATA_SIZE) {
        /* XXX error condition */
        valsize = NAME_DATA_SIZE;
    }
    if (value != 0) {
        memcpy(np->namedata, value, valsize);
    }

    return np;

} /* name_declare */