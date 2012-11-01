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
#include "utils.h"

struct scopectx_s;
typedef struct scopectx_s *scopectx_t;

#define NAME_SIZE       32

#define NAME_M_RESERVED (1<<0) // error if the name is redefined
#define NAME_M_QFUNC    (1<<1) // %QUOTE/%UNQUOTE/%EXPAND

typedef enum {
    NAMETYPE_KEYWORD,
    NAMETYPE_LEXFUNC,
    NAMETYPE_LEXNAME,
    NAMETYPE_UNDECLARED
} nametype_t;

typedef enum {
    LNTYPE_NONE = 0,
    LNTYPE_MACRO,
    LNTYPE_MACPARAM,
    LNTYPE_COMPILETIME,
    LNTYPE_LITERAL
} lntype_t;

struct name_s {
    struct name_s       *next;
    nametype_t           nametype;
    lntype_t             name_lntype;
    scopectx_t           namescope;
    unsigned int         nameflags;
    data_t               namedata;
    size_t               namelen;
    char                 name[NAME_SIZE];
};
// NB: max name length is (NAME_SIZE-1)

/*
 * Macros for building static tables of reserved keywords
 * and predeclared names
 */
#define KWDDEF(kwd, data) { 0, NAMETYPE_KEYWORD, 0, 0, \
                            NAME_M_RESERVED,  {(void *)(data)}, \
                            sizeof(kwd)-1, (kwd) }
#define LEXDEF(kwd, data, f) { 0, NAMETYPE_LEXFUNC, 0, 0, \
                            (NAME_M_RESERVED|(f)), {(void *)(data)}, \
                            sizeof(kwd)-1, (kwd) }
#define PREDEF(kwd, data) { 0, NAMETYPE_KEYWORD, 0, 0,\
                            0, {(void *)(data)}, \
                            sizeof(kwd)-1, (kwd) }

typedef struct name_s name_t;

name_t *name_search(scopectx_t scope, const char *id,
                    size_t len, int do_create);
void name_insert(scopectx_t scope, name_t *name);
void name_free(name_t *name);
scopectx_t scope_begin (scopectx_t parent);
scopectx_t scope_end (scopectx_t scope);

#endif
