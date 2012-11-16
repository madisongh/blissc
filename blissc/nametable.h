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
#include "lexeme.h"
#include "strings.h"
#include "utils.h"

struct scopectx_s;
typedef struct scopectx_s *scopectx_t;

#define NAME_SIZE       32

#define NAME_M_RESERVED (1<<0) // error if the name is redefined

struct name_s {
    struct name_s       *next;
    lextype_t            nametype;
    scopectx_t           namescope;
    unsigned int         nameflags;
    data_t               namedata;
    size_t               namelen;
    char                 name[NAME_SIZE];
};
// NB: max name length is (NAME_SIZE-1)
typedef struct name_s name_t;

/*
 * Macros for building static tables of reserved keywords
 * and predeclared names
 */
#define NAMEDEF(n_, lt_, d_, f_) { 0, (lt_), 0, \
                            (f_),  {(void *)(d_)}, \
                            sizeof(n_)-1, (n_) }

static inline __unused lextype_t name_type(name_t *name) {
    return name->nametype;
}
static inline __unused scopectx_t name_scope(name_t *name) {
    return name->namescope;
}
static inline __unused unsigned int name_flags(name_t *name) {
    return name->nameflags;
}
static inline __unused strdesc_t *name_string(name_t *name) {
    return string_from_chrs(0, name->name, name->namelen);;
}
static inline __unused void *name_val_ptr(name_t *name) {
    return name->namedata.ptr;
}
static inline __unused unsigned long name_val_unsigned(name_t *name)
{
    return name->namedata.val_unsigned;
}
static inline __unused long name_val_signed (name_t *name) {
    return name->namedata.val_signed;
}

name_t *name_search(scopectx_t scope, const char *id,
                    size_t len, int do_create);
void name_insert(scopectx_t scope, name_t *name);
void name_free(name_t *name);
name_t *name_declare(scopectx_t scope, const char *id,
                     size_t len, lextype_t type, data_t *data);
scopectx_t scope_begin(scopectx_t parent);
scopectx_t scope_end(scopectx_t scope);
scopectx_t scope_copy(scopectx_t src, scopectx_t newparent);
scopectx_t scope_getparent(scopectx_t scope);
void scope_setparent(scopectx_t scope, scopectx_t newparent);
#endif
