#ifndef symbols_h__
#define symbols_h__
/*
 *++
 * symbols.h - Definitions for symbol names
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "expression.h"
#include "nametable.h"
#include "lexeme.h"

typedef enum {
    SYMSCOPE_LOCAL,
    SYMSCOPE_GLOBAL,
    SYMSCOPE_EXTERNAL
} symscope_t;

typedef enum {
    DCLASS_UNKNOWN,   // for forwards
    DCLASS_STATIC,
    DCLASS_REGISTER,
    DCLASS_STKORREG,
    DCLASS_STACKONLY,
    DCLASS_ARG        // subject to linkage
} dataclass_t;

#define SYM_M_VOLATILE  (1<<0)
#define SYM_M_ALIAS     (1<<1)
#define SYM_M_NOVALUE   (1<<2)
#define SYM_M_REF       (1<<3)
#define SYM_M_SIGNEXT   (1<<4)
#define SYM_M_RESERVED  (1<<5)
#define SYM_M_PENDING   (1<<6)
#define SYM_M_FORWARD   (1<<7)
#define SYM_M_BIND      (1<<8)
#define SYM_M_PLIT      (1<<9)

#define PSECT_M_ATTR_WRITE   (1<<13)
#define PSECT_M_ATTR_EXEC    (1<<14)
#define PSECT_M_ATTR_OVERLAY (1<<15)

struct symctx_s;
typedef struct symctx_s *symctx_t;

struct literal_attr_s {
    unsigned long value;
    unsigned int  width;
    unsigned int  flags;
    symscope_t    sc;
};
typedef struct literal_attr_s literal_attr_t;

struct data_attr_s {
    dataclass_t        dclass;
    unsigned int      units;
    name_t            *struc;
    scopectx_t         struscope;
    name_t            *owner;
    initval_t         *ivlist;
    namereflist_t     fields;
    unsigned int      flags;
    unsigned int      alignment;
    unsigned int      width;
    symscope_t        sc;
};
typedef struct data_attr_s data_attr_t;

struct routine_attr_s {
    namereflist_t       inargs;
    namereflist_t       outargs;
    scopectx_t          argscope;
    name_t             *owner;
    unsigned int        flags;
    symscope_t          sc;
    initval_t          *ivlist;
};
typedef struct routine_attr_s routine_attr_t;

// Initval structure - for handling compile-time
// initialization of PLITs and data segments (via INITIAL
// and PRESET).
struct initval_s {
    struct initval_s *next;
    struct initval_s *lastptr;
    enum { IVTYPE_SCALAR, IVTYPE_EXPR_EXP,
           IVTYPE_STRING, IVTYPE_LIST } type;
    unsigned int repcount;
    void *preset_expr;
    union {
        struct {
            void           *expr;
            long            value;
            unsigned int    width;
            int             signext;
        } scalar;
        strdesc_t           *string;
        struct initval_s    *listptr;
    } data;
};

typedef unsigned int (*sym_gensize_fn)(void *ctx, lextype_t lt);
typedef int (*sym_geninit_fn)(void *ctx, name_t *np, void *p);
typedef int (*sym_generator_fn)(void *ctx, name_t *np, void *p);
typedef void (*sym_genfree_fn)(void *ctx, name_t *np, void *p);
typedef int (*sym_gencopy_fn)(void *ctx, name_t *dnp, void *dp,
                              name_t *snp, void *sp);

struct sym_genvec_s {
    sym_gensize_fn      sizefn;
    sym_generator_fn    genfn;
    sym_geninit_fn      geninit;
    sym_genfree_fn      genfree;
    sym_gencopy_fn      gencopy;

};
typedef struct sym_genvec_s sym_genvec_t;

symctx_t symbols_init(expr_ctx_t ctx);
void symbols_connect_hooks(symctx_t ctx);
void symbols_finish(symctx_t ctx);
void symbols_gen_register(symctx_t ctx, void *genctx, sym_genvec_t *vec);
name_t *datasym_search(scopectx_t scope, strdesc_t *namedsc,
                       data_attr_t *attrp);
name_t *datasym_declare(scopectx_t scope, strdesc_t *dsc,
                        data_attr_t *attr, textpos_t pos);
int datasym_attr_update(name_t *np, data_attr_t *attr);
data_attr_t *datasym_attr(name_t *np);
name_t *compiletime_declare(scopectx_t scope, strdesc_t *dsc,
                            long val, textpos_t pos);
void compiletime_assign(name_t *np, long val);
long compiletime_value(name_t *np);
name_t *label_declare(scopectx_t scope, strdesc_t *dsc, textpos_t pos);
expr_node_t *label_block(name_t *np);
void label_block_set(name_t *np, expr_node_t *b);
name_t *litsym_search(scopectx_t scope, strdesc_t *dsc, unsigned long *valp);
name_t *litsym_declare(scopectx_t scope, strdesc_t *dsc,
                       literal_attr_t *attr, textpos_t pos);
literal_attr_t *litsym_attr(name_t *np);
name_t *litsym_special(scopectx_t scope, strdesc_t *dsc,
                       unsigned long value);
name_t *rtnsym_search(scopectx_t scope, strdesc_t *dsc);
name_t *rtnsym_declare(scopectx_t scope, strdesc_t *dsc,
                       routine_attr_t *attr, textpos_t pos);
int rtnsym_attr_update(name_t *np, routine_attr_t *attr);
expr_node_t *rtnsym_expr(name_t *np);
void rtnsym_expr_set(name_t *np, expr_node_t *exp);
routine_attr_t *rtnsym_attr(name_t *np);

name_t *modsym_declare(scopectx_t scope, strdesc_t *str, textpos_t pos);
strdesc_t *modsym_ident(name_t *np);
void modsym_ident_set(name_t *np, strdesc_t *str);
strdesc_t *modsym_main(name_t *np);
void modsym_main_set(name_t *np, strdesc_t *str);
expr_node_t *modsym_block(name_t *np);
void modsym_block_set(name_t *np, expr_node_t *blk);
int sym_undeclare(scopectx_t scope, strdesc_t *dsc, textpos_t pos);
int sym_addrs_comparable(name_t *np_a, name_t *np_b);
void sym_check_dangling_forwards(scopectx_t scope, textpos_t pos);
void *sym_genspace(name_t *np);
name_t *psect_declare(scopectx_t scope, strdesc_t *dsc,
                      unsigned int psflags, textpos_t pos);
name_t *psect_search(scopectx_t scope, strdesc_t *namedsc);
unsigned int psect_attr(name_t *np);

initval_t *initval_scalar_add(symctx_t ctx, initval_t *head, unsigned int reps,
                              long val, unsigned int width, int signext);
initval_t *initval_expr_add(symctx_t ctx, initval_t *head, unsigned int reps,
                            void *exp, unsigned int width, int signext);
initval_t *initval_scalar_prepend(symctx_t ctx, initval_t *head, unsigned int reps,
                                  long val, unsigned int width, int signext);
initval_t *initval_string_add(symctx_t ctx, initval_t *head, unsigned int reps,
                              strdesc_t *str);
initval_t *initval_ivlist_add(symctx_t ctx, initval_t *head, unsigned int reps,
                              initval_t *sublist);
void initval_freelist(symctx_t ctx, initval_t *iv);
unsigned long initval_size(symctx_t ctx, initval_t *ivlist);
initval_t *preset_scalar_add(symctx_t ctx, initval_t *head, void *pexp, long val);
initval_t *preset_expr_add(symctx_t ctx, initval_t *head, void *pexp, void *exp);

#endif /* symbols_h__ */
