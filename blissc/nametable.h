//
//  nametable.h
//  blissc
//
//  Created by Matthew Madison on 10/27/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_nametable_h
#define blissc_nametable_h

#include <string.h>
#include <stdint.h>
#include "lexeme.h"
#include "utils.h"
#include "strings.h"

typedef enum {
    SCLASS_OWN,
    SCLASS_GLOBAL,
    SCLASS_PLIT,
    SCLASS_CODE,
} storageclass_t;
#define SCLASS_COUNT 4

struct scopectx_s;
typedef struct scopectx_s *scopectx_t;

#define NAME_SIZE       32

#define NAME_M_RESERVED (1<<0) // error if the name is redefined
#define NAME_M_DECLARED (1<<1) // set if explicitly declared
#define NAME_M_FLAGMASK (0xFFFF) // others are reserved for internal use

struct name_s {
    struct name_s       *next;
    lextype_t            nametype;
    scopectx_t           namescope;
    textpos_t            namedclpos;
    unsigned int         nameflags;
    union {
        void            *ptr;
        long             val;
        lexseq_t         lexseq;
    }                    nameunion;
    size_t               namelen;
    char                 name[NAME_SIZE];
};
// NB: max name length is (NAME_SIZE-1)
typedef struct name_s name_t;

typedef void (*name_datafree_fn)(name_t *np);
typedef int (*name_datacopy_fn)(name_t *dst, name_t *src);

/*
 * Macros for building static tables of reserved keywords
 * and predeclared names
 */
#define NAMEDEF(n_, lt_, f_) { 0, (lt_), 0, 0,	\
                            (f_), { 0 }, sizeof(n_)-1, (n_) }

static inline __unused lextype_t name_type(name_t *name) {
    return name->nametype;
}
static inline __unused scopectx_t name_scope(name_t *name) {
    return name->namescope;
}
static inline __unused unsigned int name_flags(name_t *name) {
    return (name->nameflags & NAME_M_FLAGMASK);
}
static inline __unused void name_flags_set(name_t *name, unsigned int f) {
    name->nameflags = (name->nameflags & ~NAME_M_FLAGMASK) | (f & NAME_M_FLAGMASK);
}
static inline __unused strdesc_t *name_string(name_t *name) {
    return string_from_chrs(0, name->name, name->namelen);;
}
static inline __unused void *name_data_ptr(name_t *name) {
    return name->nameunion.ptr;
}
static inline __unused void name_data_set_ptr(name_t *name, void *ptr) {
    name->nameunion.ptr = ptr;
}
static inline __unused lexseq_t *name_data_lexseq(name_t *name) {
    return &name->nameunion.lexseq;
}
static inline __unused void name_data_set_lexseq(name_t *name, lexseq_t *seq) {
    lexseq_init(&name->nameunion.lexseq);
    if (seq != 0) lexseq_copy(&name->nameunion.lexseq, seq);
}
static inline __unused long name_data_int(name_t *name) {
    return name->nameunion.val;
}
static inline __unused void name_data_set_int(name_t *name, long val) {
    name->nameunion.val = val;
}
static inline __unused textpos_t name_dclpos_get(name_t *name) {
    return name->namedclpos;
}
static inline __unused void name_dclpos_set(name_t *name, textpos_t pos) {
    name->namedclpos = pos;
}

name_t *name_search(scopectx_t scope, const char *id,
                    size_t len, int do_create);
void name_insert(scopectx_t scope, name_t *name);
void name_free(name_t *name);
name_t *name_declare(scopectx_t scope, const char *id,
                     size_t len, lextype_t type, textpos_t pos);
name_t *name_declare_nocheck(scopectx_t scope, const char *id,
                             size_t len, lextype_t type, textpos_t pos);
scopectx_t scope_begin(scopectx_t parent);
scopectx_t scope_end(scopectx_t scope);
scopectx_t scope_copy(scopectx_t src, scopectx_t newparent);
scopectx_t scope_getparent(scopectx_t scope);
name_t *scope_sclass_psectname(scopectx_t scope, storageclass_t cl);
void scope_sclass_psectname_set(scopectx_t scope, storageclass_t cl, name_t *np);
void scope_setparent(scopectx_t scope, scopectx_t newparent);
void nametype_dataop_register(lextype_t lt, name_datafree_fn fn,
                              name_datacopy_fn);
int name_undeclare(scopectx_t scope, name_t *np);
#endif
