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

#undef DONTYPE
#define DONTYPES \
DONTYPE(LITERAL) DONTYPE(GLOBLIT) DONTYPE(EXTLIT) \
DONTYPE(OWN) DONTYPE(GLOBAL) DONTYPE(LOCAL) \
DONTYPE(STACKLOCAL) DONTYPE(REGISTER) \
DONTYPE(GLOBREG) DONTYPE(EXTREG) \
DONTYPE(BIND) DONTYPE(GLOBBIND) \
DONTYPE(FORWARD) DONTYPE(EXTERNAL)

#define DONTYPE(t_) NAMETYPE_##t_,
typedef enum {
    DONTYPES
} nametype_t;
#undef DONTYPE

#define NI_M_VOLATILE (1<<0)
#define NI_M_ALIAS    (1<<1)
#define NI_M_NOVALUE  (1<<2)
#define NI_M_REF      (1<<3)

struct nameinfo_s;

struct scalar_attr_s {
    unsigned int    units;
    int             signext;
};
typedef struct scalar_attr_s scalar_attr_t;

struct ni_literal_s {
    unsigned long value;
    unsigned int  nbits;
    int           signext;
};

struct ni_gxlit_s {
    seg_t           *seg;
    unsigned int    nbits;      //XXX not sure
    int             signext;    //XXX we need these
};

struct ni_data_s {
    void            *ptr;    
    strudef_t       *struc;
    scopectx_t       struscope;
    lexseq_t         fields;
    scalar_attr_t    attr;
    unsigned int     flags;
};

struct ni_routine_s {
    expr_node_t     *rtnexp;
    frame_t         *stack;
    // also linkage
    unsigned int     flags;
};

struct nameinfo_s {
    struct nameinfo_s *next;
    nametype_t  type;
    union {
        struct ni_literal_s litinfo; // LITERAL
        struct ni_gxlit_s gxlinfo;   // GLOBLIT, EXTLIT
        struct ni_data_s  datainfo;  // data segments, BINDs
        struct ni_routine_s rtninfo;
    } data;
};
typedef struct nameinfo_s nameinfo_t;

#define siu static inline __unused
siu unsigned int scattr_units(scalar_attr_t *attr) { return attr->units; }
siu int scattr_signed(scalar_attr_t *attr) { return attr->signext; }
siu void scattr_units_set(scalar_attr_t *attr, unsigned int u) { attr->units = u; }
siu void scattr_signed_set(scalar_attr_t *attr, int s) { attr->signext = s; }
siu void scattr_copy(scalar_attr_t *dst, scalar_attr_t *src) {
    dst->units = src->units; dst->signext = src->signext;
}
siu nametype_t nameinfo_type(nameinfo_t *ni) { return ni->type; }
siu void nameinfo_type_set(nameinfo_t *ni, nametype_t t) { ni->type = t; }
siu nameinfo_t *nameinfo_next(nameinfo_t *ni) { return ni->next; }
siu void nameinfo_next_set(nameinfo_t *ni, nameinfo_t *next) { ni->next = next; }
siu unsigned long nameinfo_lit_val(nameinfo_t *ni) { return ni->data.litinfo.value; }
siu void nameinfo_lit_val_set(nameinfo_t *ni, unsigned long val) { ni->data.litinfo.value = val; }
siu unsigned int nameinfo_lit_valwidth(nameinfo_t *ni) { return ni->data.litinfo.nbits; }
siu void nameinfo_lit_valwidth_set(nameinfo_t *ni, unsigned int val) { ni->data.litinfo.nbits = val; }
siu int nameinfo_lit_signextend(nameinfo_t *ni) { return ni->data.litinfo.signext; }
siu void nameinfo_lit_signextend_set(nameinfo_t *ni, int v) { ni->data.litinfo.signext = v; }
siu seg_t *nameinfo_gxlit_seg(nameinfo_t *ni) { return ni->data.gxlinfo.seg; }
siu void nameinfo_gxlit_seg_set(nameinfo_t *ni, seg_t *seg) { ni->data.gxlinfo.seg = seg; }
siu unsigned int nameinfo_gxlit_valwidth(nameinfo_t *ni) { return ni->data.gxlinfo.nbits; }
siu void nameinfo_gxlit_valwidth_set(nameinfo_t *ni, unsigned int val) { ni->data.gxlinfo.nbits = val; }
siu int nameinfo_gxlit_signextend(nameinfo_t *ni) { return ni->data.gxlinfo.signext; }
siu void nameinfo_gxlit_signextend_set(nameinfo_t *ni, int v) { ni->data.gxlinfo.signext = v; }
siu seg_t *nameinfo_data_seg(nameinfo_t *ni) { return ni->data.datainfo.ptr; }
siu void nameinfo_data_seg_set(nameinfo_t *ni, seg_t *seg) { ni->data.datainfo.ptr = seg; }
siu strudef_t *nameinfo_data_struc(nameinfo_t *ni) {return ni->data.datainfo.struc; }
siu void nameinfo_data_struc_set(nameinfo_t *ni, strudef_t *s) { ni->data.datainfo.struc = s; }
siu scopectx_t nameinfo_data_struscope(nameinfo_t *ni) {return ni->data.datainfo.struscope; }
siu void nameinfo_data_struscope_set(nameinfo_t *ni, scopectx_t s) { ni->data.datainfo.struscope = s; }
siu lexseq_t *nameinfo_data_fields(nameinfo_t *ni) { return &ni->data.datainfo.fields; }
siu void nameinfo_data_fields_set(nameinfo_t *ni, lexseq_t *f) { lexseq_copy(&ni->data.datainfo.fields, f); }
siu scalar_attr_t *nameinfo_data_scattr(nameinfo_t *ni) { return &ni->data.datainfo.attr; }
siu void nameinfo_data_scattr_set(nameinfo_t *ni, scalar_attr_t *attr) { scattr_copy(&ni->data.datainfo.attr, attr); }
siu unsigned int nameinfo_data_flags(nameinfo_t *ni) { return ni->data.datainfo.flags; }
siu void nameinfo_data_flags_set(nameinfo_t *ni, unsigned int f) { ni->data.datainfo.flags = f; }
siu expr_node_t *nameinfo_bind_expr(nameinfo_t *ni) { return ni->data.datainfo.ptr; }
siu void nameinfo_bind_expr_set(nameinfo_t *ni, expr_node_t *exp) { ni->data.datainfo.ptr = exp; }
siu strudef_t *nameinfo_bind_struc(nameinfo_t *ni) { return ni->data.datainfo.struc; }
siu void nameinfo_bind_struc_set(nameinfo_t *ni, strudef_t *s) { ni->data.datainfo.struc = s; }
siu lexseq_t *nameinfo_bind_fields(nameinfo_t *ni) { return &ni->data.datainfo.fields; }
siu void nameinfo_bind_fields_set(nameinfo_t *ni, lexseq_t *f) { lexseq_copy(&ni->data.datainfo.fields, f); }
siu scalar_attr_t *nameinfo_bind_scattr(nameinfo_t *ni) { return &ni->data.datainfo.attr; }
siu void nameinfo_bind_scattr_set(nameinfo_t *ni, scalar_attr_t *a) { scattr_copy(&ni->data.datainfo.attr, a); }
siu unsigned int nameinfo_bind_flags(nameinfo_t *ni) { return ni->data.datainfo.flags; }
siu void nameinfo_bind_flags_set(nameinfo_t *ni, unsigned int f) { ni->data.datainfo.flags = f; }
siu expr_node_t *nameinfo_routine_expr(nameinfo_t *ni) { return ni->data.rtninfo.rtnexp; }
siu void nameinfo_routine_expr_set(nameinfo_t *ni, expr_node_t *exp) { ni->data.rtninfo.rtnexp = exp; }
siu frame_t *nameinfo_routine_stack(nameinfo_t *ni) { return ni->data.rtninfo.stack; }
siu void nameinfo_routine_stack_set(nameinfo_t *ni, frame_t *stk) { ni->data.rtninfo.stack = stk; }
#undef siu

nameinfo_t *nameinfo_alloc(nametype_t type);
void nameinfo_free(nameinfo_t *ni, stgctx_t stg);

void declarations_init(scopectx_t kwdscope, stgctx_t stg,
                       machinedef_t *mach);
seg_t *define_plit(parse_ctx_t pctx, stgctx_t stg, lextype_t curlt);
int parse_decl_name(parse_ctx_t pctx, scopectx_t scope,
                    strdesc_t **result, textpos_t *pos);
int parse_declaration(parse_ctx_t pctx);
int declare_module(parse_ctx_t pctx);
#endif
