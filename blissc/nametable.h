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

#define NAME_SIZE       32
#define NAME_M_RSVDKWD  (1<<0)
struct name_s {
    struct name_s       *next;
    void                *namedata;
    size_t               namelen;
    unsigned int         nameflags;
    char                 name[NAME_SIZE];
};
// NB: max name length is (NAME_SIZE-1)

/*
 * Macros for building static tables of reserved keywords
 * and predeclared names
 */
#define KWDDEF(kwd, data) { 0, (void *)(data), \
                            sizeof(kwd)-1, NAME_M_RSVDKWD, \
                            (kwd) }
#define PREDEF(kwd, data) { 0, (void *)(data), \
                            sizeof(kwd)-1, 0, (kwd) }


typedef struct name_s name_t;
struct scopectx_s;
typedef struct scopectx_s *scopectx_t;

name_t *name_search(scopectx_t scope, const char *id,
                    size_t len);
void name_insert(scopectx_t scope, name_t *name);
scopectx_t scope_begin (scopectx_t parent);
scopectx_t scope_end (scopectx_t scope);

#endif
