//
//  nametable.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "nametable.h"

/*
 * Internal flag for distinguishing dynamically-allocated
 * name blocks from others
 */
#define NAME_M_ALLOCATED (1<<16)

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
    np->nametype = NAMETYPE_UNDECLARED;
    memset(&np->namedata, 0, sizeof(np->namedata));
    return np;

} /* name_alloc */

/*
 * name_free
 *
 * Frees a name_t structure, if it's owned by this module,
 * and if the name is not referenced in any scope.
 */
void
name_free (name_t *np)
{
    if (np->namescope == 0 &&
        (np->nameflags & NAME_M_ALLOCATED)) {
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
    scopectx_t parent = scope->parent;

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
                if (np->nametype != NAMETYPE_UNDECLARED &&
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
              nametype_t type, data_t *data)
{
    name_t *np;

    if (len >= NAME_SIZE) {
        len = NAME_SIZE-1;
    }

    np = name_search(scope, id, len, 0);
    if (np != 0) {
        if (np->namescope == scope && np->nametype != NAMETYPE_UNDECLARED) {
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
    if (data != 0) {
        memcpy(&np->namedata, data, sizeof(data_t));
    }
    return np;

} /* name_declare */