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
struct namectx_s;
typedef struct namectx_s *namectx_t;
struct name_s;
typedef struct name_s name_t;

struct nameref_s {
    TQ_ENT_FIELDS(struct nameref_s)
    struct name_s    *np;
};
typedef struct nameref_s nameref_t;

struct namereflist_s {
    TQ_HDR_FIELDS(nameref_t)
};
typedef struct namereflist_s namereflist_t;

#define NAME_SIZE       32

#define NAME_M_RESERVED (1<<0) // error if the name is redefined
#define NAME_M_DECLARED (1<<1) // set if explicitly declared
#define NAME_M_FLAGMASK (0xFFFF) // others are reserved for internal use

DEFINE_TQ_FUNCS(namereflist, namereflist_t, nameref_t)

typedef int (*name_datainit_fn)(void *ctx, name_t *np, void *p);
typedef void (*name_datafree_fn)(void *ctx, name_t *np, void *p);
typedef int (*name_datacopy_fn)(void *ctx, name_t *dst, void *dp, name_t *src, void *sp);

struct nametype_vectors_s {
    size_t              typesize;
    void               *typectx;
    name_datainit_fn    typeinit;
    name_datafree_fn    typefree;
    name_datacopy_fn    typecopy;
};
typedef struct nametype_vectors_s nametype_vectors_t;

/*
 * Macros for building static tables of reserved keywords
 * and predeclared names
 */
struct namedef_s {
    lextype_t       lt;
    unsigned int    flags;
    size_t          namelen;
    char            *name;
};
typedef struct namedef_s namedef_t;
#define NAMEDEF(n_, lt_, f_) { (lt_), (f_), sizeof(n_)-1, (n_) }

name_t *name_search(scopectx_t scope, const char *id,
                    size_t len, lextype_t *ntypep);
name_t *name_search_typed(scopectx_t scope, const char *id,
                          size_t len, lextype_t ntype, void *datapp);
void name_insert(scopectx_t scope, name_t *name);
void name_free(name_t *name);
name_t *name_declare(scopectx_t scope, namedef_t *def, textpos_t pos,
                     void *datap, size_t datasize, void *datapp);
name_t *name_declare_nocheck(scopectx_t scope, namedef_t *def, textpos_t pos,
                             void *datap, size_t datasize, void *datapp);
lextype_t name_type(name_t *np);
void *name_extraspace(name_t *np);
void *name_value_pointer(name_t *np);
long name_value_signed(name_t *np);
unsigned long name_value_unsigned(name_t *np);
void name_value_pointer_set(name_t *np, void *ptr);
void name_value_signed_set(name_t *np, long val);
void name_value_unsigned_set(name_t *np, unsigned long val);
int name_is_declared(scopectx_t scope, const char *id, size_t len);
scopectx_t name_scope(name_t *np);
lexeme_t *name_to_lexeme(lexctx_t lctx, name_t *np, textpos_t pos);
strdesc_t *name_string(name_t *np);
namectx_t nametables_init(void);
scopectx_t nametables_globalscope(namectx_t ctx);
void *nametables_symctx_get(namectx_t ctx);
void nametables_symctx_set(namectx_t ctx, void *p);
scopectx_t scope_begin(namectx_t ctx, scopectx_t parent);
scopectx_t scope_end(scopectx_t scope);
scopectx_t scope_copy(scopectx_t src, scopectx_t newparent);
scopectx_t scope_getparent(scopectx_t scope);
namectx_t scope_namectx(scopectx_t scope);
name_t *scope_sclass_psectname(scopectx_t scope, storageclass_t cl);
void scope_sclass_psectname_set(scopectx_t scope, storageclass_t cl, name_t *np);
void scope_setparent(scopectx_t scope, scopectx_t newparent);
void nametype_dataop_register(namectx_t ctx, lextype_t lt,
                              nametype_vectors_t *vec, void *vctx);
int name_undeclare(scopectx_t scope, name_t *np);
strdesc_t *tempname_get(namectx_t ctx);

nameref_t *nameref_alloc(namectx_t ctx, name_t *np);
void nameref_free(namectx_t ctx, nameref_t *nr);
void namereflist_free(namectx_t ctx, namereflist_t *reflist);
#endif
