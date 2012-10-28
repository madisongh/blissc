//
//  nametable.c
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "nametable.h"

#define NAME_M_ALLOCATED (1<<16)

#define HT_BUCKETS  32

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

struct scopectx_s {
    struct scopectx_s *parent;
    int                namecount;
    struct name_s     *hashtable[HT_BUCKETS];
};

struct scopectx_s *freescopes = 0;
struct name_s     *freenames = 0;

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
    if (namelen > NAME_SIZE-1) {
        namelen = NAME_SIZE-1;
    }
    memcpy(np->name, name, namelen);
    np->name[namelen] = '\0';
    np->nameflags = NAME_M_ALLOCATED;
    np->namelen = namelen;
    np->namedata = 0;
    return np;
}

static void
name_free (name_t *np)
{
    if (np->nameflags & NAME_M_ALLOCATED) {
        np->next = freenames;
        freenames = np;
    }
}

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
}

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
                name_free(name);
                name = next;
            }
        }
    }
    scope->parent = freescopes;
    freescopes = scope;
    return parent;
}

name_t *
name_search (scopectx_t scope, const char *id, size_t len)
{
    int i;
    name_t *np;

    while (scope != 0) {
        if (scope->namecount > 0) {
            i = hash(id, len);
            for (np = scope->hashtable[i]; np != 0;
                 np = np->next) {
                if (len == np->namelen &&
                    memcmp(id, np->name, len)) {
                    return np;
                }
            }
        }
        scope = scope->parent;
    }

    return 0;
}

void
name_insert (scopectx_t scope, name_t *np)
{
    int i;

    // callers should ensure that this never happens
    if (np->namelen >= NAME_SIZE)
        np->namelen = NAME_SIZE-1;

    i = hash(np->name, np->namelen);
    np->next = scope->hashtable[i];
    scope->hashtable[i] = np;

    scope->namecount += 1;

}