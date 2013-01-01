#ifndef symbols_h__
#define symbols_h__
/*
 *++
 *	File:			symbols.h
 *
 *	Abstract:		Definitions for symbol names
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "expression.h"
#include "storage.h"
#include "nametable.h"
#include "lexeme.h"

typedef enum {
    SYMSCOPE_LOCAL,
    SYMSCOPE_GLOBAL,
    SYMSCOPE_EXTERNAL
} symscope_t;

#define SYM_M_VOLATILE  (1<<0)
#define SYM_M_ALIAS     (1<<1)
#define SYM_M_NOVALUE   (1<<2)
#define SYM_M_REF       (1<<3)
#define SYM_M_SIGNEXT   (1<<4)
#define SYM_M_STACKONLY (1<<5)
#define SYM_M_RESERVED  (1<<6)
#define SYM_M_PENDING   (1<<7)
#define SYM_M_FORWARD   (1<<8)
#define SYM_M_ARG       (1<<9)

struct literal_attr_s {
    unsigned long value;
    unsigned int  width;
    unsigned int  flags;
};
typedef struct literal_attr_s literal_attr_t;

struct data_attr_s {
    unsigned int      units;
    name_t            *struc;
    scopectx_t         struscope;
    name_t            *psect;
    namereflist_t     fields;
    unsigned int      flags;
};
typedef struct data_attr_s data_attr_t;

struct routine_attr_s {
    namereflist_t       inargs;
    namereflist_t       outargs;
    scopectx_t          argscope;
    frame_t            *stackframe;
    unsigned int        flags;
};
typedef struct routine_attr_s routine_attr_t;

void symbols_init(expr_ctx_t ctx, gencodectx_t gctx);
name_t *datasym_search(scopectx_t scope, strdesc_t *namedsc,
                       data_attr_t *attrp);
unsigned long datasym_segsize(name_t *np);
name_t *datasym_declare(scopectx_t scope, strdesc_t *dsc,
                        symscope_t sc, data_attr_t *attr,
                        textpos_t pos);
seg_t *datasym_seg(name_t *np);
void datasym_seg_set(name_t *np, seg_t *seg);
void *datasym_genref(name_t *np);
void datasym_genref_set(name_t *np, void *ref);
int datasym_attr_update(name_t *np, data_attr_t *attr);
data_attr_t *datasym_attr(name_t *np);
name_t *compiletime_declare(scopectx_t scope, strdesc_t *dsc,
                            long val, textpos_t pos);
void compiletime_assign(name_t *np, long val);
long compiletime_value(name_t *np);
name_t *label_declare(scopectx_t scope, strdesc_t *dsc, textpos_t pos);
expr_node_t *label_block(name_t *np);
void label_block_set(name_t *np, expr_node_t *b);
void *label_genref(name_t *np);
void label_genref_set(name_t *np, void *p);
void *label_exitpoint(name_t *np);
void label_exitpoint_set(name_t *np, void *p);
name_t *litsym_search(scopectx_t scope, strdesc_t *dsc, unsigned long *valp);
name_t *litsym_declare(scopectx_t scope, strdesc_t *dsc,
                       symscope_t sc, literal_attr_t *attr,
                       textpos_t pos);
void *litsym_genref(name_t *np);
void litsym_genref_set(name_t *np, void *ref);
literal_attr_t *litsym_attr(name_t *np);
name_t *litsym_special(scopectx_t scope, strdesc_t *dsc,
                       unsigned long value);
name_t *rtnsym_search(scopectx_t scope, strdesc_t *dsc);
name_t *rtnsym_declare(scopectx_t scope, strdesc_t *dsc,
                       symscope_t sc, routine_attr_t *attr,
                       textpos_t pos);
int rtnsym_attr_update(name_t *np, routine_attr_t *attr);
expr_node_t *rtnsym_expr(name_t *np);
void rtnsym_expr_set(name_t *np, expr_node_t *exp);
seg_t *rtnsym_seg(name_t *np);
void rtnsym_seg_set(name_t *np, seg_t *seg);
routine_attr_t *rtnsym_attr(name_t *np);
void *rtnsym_genref(name_t *np);
void rtnsym_genref_set(name_t *np, void *ref);

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
name_t *psect_declare(scopectx_t scope, strdesc_t *dsc,
                      unsigned int psflags, textpos_t pos);
psect_t *psect_search(scopectx_t scope, strdesc_t *namedsc);
psect_t *psect_pointer(name_t *np);

#endif /* symbols_h__ */
