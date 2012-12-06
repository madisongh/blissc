//
//  declarations.h
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_declarations__h
#define blissc_declarations__h

#include "structures.h"
#include "expression.h"
#include "nametable.h"
#include "storage.h"
#include "parser.h"
#include "lexeme.h"
#include "machinedef.h"

#undef DOSYMTYPE
#define DOSYMTYPES \
DOSYMTYPE(LITERAL) \
DOSYMTYPE(DATASEG) \
DOSYMTYPE(REGISTER) \
DOSYMTYPE(ROUTINE)

#define DOSYMTYPE(t_) SYMTYPE_##t_,
typedef enum {
    DOSYMTYPES
} symtype_t;
#undef DOSYMTYPE

#define SYM_M_VOLATILE (1<<0)
#define SYM_M_ALIAS    (1<<1)
#define SYM_M_NOVALUE  (1<<2)
#define SYM_M_REF      (1<<3)

struct symbol_s;

struct scalar_attr_s {
    unsigned int    units;
    int             signext;
};
typedef struct scalar_attr_s scalar_attr_t;

#define ARGLIST_K_MAXARGS  16
struct arglist_s {
    name_t      *arg[ARGLIST_K_MAXARGS];
    int          count;
};
typedef struct arglist_s arglist_t;

struct sym_literal_s {
    unsigned long value;
    unsigned int  nbits;
    int           signext;
    int           hasvalue;
};

struct sym_data_s {
    seg_t           *seg;
    strudef_t       *struc;
    scopectx_t       struscope;
    lexseq_t         fields;
    scalar_attr_t    attr;
    unsigned int     flags;
};

struct sym_routine_s {
    expr_node_t     *rtnexp;
    frame_t         *stack;
    seg_t           *seg;
    scopectx_t       argscope;
    arglist_t        inpargs;
    arglist_t        outargs;
    // also linkage
    unsigned int     flags;
};

struct symbol_s {
    struct symbol_s *next;
    symtype_t  type;
    union {
        struct sym_literal_s litinfo; // LITERALs
        struct sym_data_s  datainfo;  // data segments, BINDs
        struct sym_routine_s rtninfo; // routines, BIND ROUTINEs
    } data;
};
typedef struct symbol_s symbol_t;

#define siu static inline __unused
siu unsigned int scattr_units(scalar_attr_t *attr) { return attr->units; }
siu int scattr_signed(scalar_attr_t *attr) { return attr->signext; }
siu void scattr_units_set(scalar_attr_t *attr, unsigned int u) { attr->units = u; }
siu void scattr_signed_set(scalar_attr_t *attr, int s) { attr->signext = s; }
siu void scattr_copy(scalar_attr_t *dst, scalar_attr_t *src) {
    dst->units = src->units; dst->signext = src->signext;
}
siu symtype_t symbol_type(symbol_t *ni) { return ni->type; }
siu void symbol_type_set(symbol_t *ni, symtype_t t) { ni->type = t; }
siu symbol_t *symbol_next(symbol_t *ni) { return ni->next; }
siu void symbol_next_set(symbol_t *ni, symbol_t *next) { ni->next = next; }
siu unsigned long symbol_lit_val(symbol_t *ni) { return ni->data.litinfo.value; }
siu void symbol_lit_val_set(symbol_t *ni, unsigned long val) { ni->data.litinfo.value = val; }
siu unsigned int symbol_lit_valwidth(symbol_t *ni) { return ni->data.litinfo.nbits; }
siu void symbol_lit_valwidth_set(symbol_t *ni, unsigned int val) { ni->data.litinfo.nbits = val; }
siu int symbol_lit_signextend(symbol_t *ni) { return ni->data.litinfo.signext; }
siu void symbol_lit_signextend_set(symbol_t *ni, int v) { ni->data.litinfo.signext = v; }
siu int symbol_lit_hasvalue(symbol_t *ni) { return ni->data.litinfo.hasvalue; }
siu void symbol_lit_hasvalue_set(symbol_t *ni, int v) { ni->data.litinfo.hasvalue = v; }
siu seg_t *symbol_data_seg(symbol_t *ni) { return ni->data.datainfo.seg; }
siu void symbol_data_seg_set(symbol_t *ni, seg_t *seg) { ni->data.datainfo.seg = seg; }
siu strudef_t *symbol_data_struc(symbol_t *ni) {return ni->data.datainfo.struc; }
siu void symbol_data_struc_set(symbol_t *ni, strudef_t *s) { ni->data.datainfo.struc = s; }
siu scopectx_t symbol_data_struscope(symbol_t *ni) {return ni->data.datainfo.struscope; }
siu void symbol_data_struscope_set(symbol_t *ni, scopectx_t s) { ni->data.datainfo.struscope = s; }
siu lexseq_t *symbol_data_fields(symbol_t *ni) { return &ni->data.datainfo.fields; }
siu void symbol_data_fields_set(symbol_t *ni, lexseq_t *f) { lexseq_copy(&ni->data.datainfo.fields, f); }
siu scalar_attr_t *symbol_data_scattr(symbol_t *ni) { return &ni->data.datainfo.attr; }
siu void symbol_data_scattr_set(symbol_t *ni, scalar_attr_t *attr) { scattr_copy(&ni->data.datainfo.attr, attr); }
siu unsigned int symbol_data_flags(symbol_t *ni) { return ni->data.datainfo.flags; }
siu void symbol_data_flags_set(symbol_t *ni, unsigned int f) { ni->data.datainfo.flags = f; }
siu expr_node_t *symbol_routine_expr(symbol_t *ni) { return ni->data.rtninfo.rtnexp; }
siu void symbol_routine_expr_set(symbol_t *ni, expr_node_t *exp) { ni->data.rtninfo.rtnexp = exp; }
siu frame_t *symbol_routine_stack(symbol_t *ni) { return ni->data.rtninfo.stack; }
siu void symbol_routine_stack_set(symbol_t *ni, frame_t *stk) { ni->data.rtninfo.stack = stk; }
siu arglist_t *symbol_routine_inpargs(symbol_t *ni) { return &ni->data.rtninfo.inpargs; }
siu arglist_t *symbol_routine_outargs(symbol_t *ni) { return &ni->data.rtninfo.outargs; }
siu seg_t *symbol_routine_seg(symbol_t *ni) { return ni->data.rtninfo.seg; }
siu void symbol_routine_seg_set(symbol_t *ni, seg_t *seg) { ni->data.rtninfo.seg = seg; }
siu scopectx_t symbol_routine_argscope(symbol_t *ni) { return ni->data.rtninfo.argscope; }
siu void symbol_routine_argscope_set(symbol_t *ni, scopectx_t s) { ni->data.rtninfo.argscope = s; }
#undef siu

symbol_t *symbol_alloc(expr_ctx_t ctx, symtype_t type);
void symbol_free(expr_ctx_t ctx, symbol_t *ni);

void *declarations_init(expr_ctx_t ctx, parse_ctx_t pctx,
                       scopectx_t kwdscope, stgctx_t stg,
                       machinedef_t *mach);
seg_t *define_plit(expr_ctx_t ctx, lextype_t curlt);
int parse_decl_name(parse_ctx_t pctx, scopectx_t scope,
                    strdesc_t **result, textpos_t *pos);
int parse_declaration(expr_ctx_t ectx);
int declare_module(expr_ctx_t ectx);
#endif
