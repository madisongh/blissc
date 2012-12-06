//
//  expression.h
//  blissc
//
//  Created by Matthew Madison on 11/1/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_expression_h
#define blissc_expression_h

#include "parser.h"
#include "storage.h"
#include "nametable.h"
#include "lexeme.h"
#include "utils.h"

#undef DOEXPTYPE
#define DOEXPTYPES \
    DOEXPTYPE(NOOP) \
    DOEXPTYPE(PRIM_LIT) DOEXPTYPE(PRIM_SEG) \
    DOEXPTYPE(PRIM_FLDREF) DOEXPTYPE(PRIM_RTNCALL) \
    DOEXPTYPE(PRIM_STRUREF) \
    DOEXPTYPE(PRIM_BLK) DOEXPTYPE(OPERATOR) \
    DOEXPTYPE(EXECFUN) DOEXPTYPE(CTRL_COND) \
    DOEXPTYPE(CTRL_CASE) DOEXPTYPE(CTRL_EXIT) \
    DOEXPTYPE(CTRL_SELECT) DOEXPTYPE(SELECTOR) \
    DOEXPTYPE(CTRL_LOOPWU) DOEXPTYPE(CTRL_LOOPID)
#define DOEXPTYPE(t_) EXPTYPE_##t_,
typedef enum {
    DOEXPTYPES
} exprtype_t;
#undef DOEXPTYPE
#define EXPTYPE_COUNT (EXPTYPE_CTRL_LOOPID+1)

#undef DOOPTYPE
#define DOOPTYPES \
    DOOPTYPE(FETCH) DOOPTYPE(UNARY_PLUS) DOOPTYPE(UNARY_MINUS) \
    DOOPTYPE(SHIFT) DOOPTYPE(MODULO) DOOPTYPE(MULT) DOOPTYPE(DIV) \
    DOOPTYPE(ADD) DOOPTYPE(SUBTRACT) \
    DOOPTYPE(CMP_EQL) DOOPTYPE(CMP_NEQ) \
    DOOPTYPE(CMP_LSS) DOOPTYPE(CMP_LEQ) \
    DOOPTYPE(CMP_GTR) DOOPTYPE(CMP_GEQ) \
    DOOPTYPE(CMP_EQLU) DOOPTYPE(CMP_NEQU) \
    DOOPTYPE(CMP_LSSU) DOOPTYPE(CMP_LEQU) \
    DOOPTYPE(CMP_GTRU) DOOPTYPE(CMP_GEQU) \
    DOOPTYPE(CMP_EQLA) DOOPTYPE(CMP_NEQA) \
    DOOPTYPE(CMP_LSSA) DOOPTYPE(CMP_LEQA) \
    DOOPTYPE(CMP_GTRA) DOOPTYPE(CMP_GEQA) \
    DOOPTYPE(NOT) DOOPTYPE(AND) DOOPTYPE(OR) \
    DOOPTYPE(EQV) DOOPTYPE(XOR) \
    DOOPTYPE(ASSIGN)
#define DOOPTYPE(t_) OPER_##t_,
typedef enum {
    DOOPTYPES
} optype_t;
#undef DOOPTYPE
#define OPER_COUNT (OPER_ASSIGN+1)
#define OPER_NONE OPER_COUNT

struct expr_ctx_s;
typedef struct expr_ctx_s *expr_ctx_t;
struct expr_node_s;

struct exprseq_s {
    struct expr_node_s  *head;
    struct expr_node_s  *tail;
    int                  count;
};
typedef struct exprseq_s exprseq_t;

struct expr_lit_s {
    long                litval;
    strdesc_t           *litstring;
};

struct expr_blk_s {
    exprseq_t           blkseq;
    struct expr_node_s *blkval;
    scopectx_t          blkscope;
    strdesc_t          *codecomment;
};
struct expr_struref_s {
    struct expr_node_s *accexpr;
};
struct expr_oper_s {
    struct expr_node_s *op_lhs, *op_rhs;
    optype_t            op_type;
};
struct expr_func_s {
    struct expr_node_s *func_arglist;
    // exfntype_t which;
};
struct expr_fldref_s {
    struct expr_node_s  *addr;
    struct expr_node_s  *pos;
    struct expr_node_s  *size;
    int                 signext;
};
struct expr_cond_s {
    struct expr_node_s  *test;
    struct expr_node_s  *cons;
    struct expr_node_s  *alt;
};
typedef enum {
    LOOP_PRETEST, LOOP_POSTTEST
} looptesttype_t;
struct expr_loopwu_s {
    struct expr_node_s  *test;
    struct expr_node_s  *body;
    looptesttype_t       type;
};
struct expr_loopid_s {
    scopectx_t          scope;
    struct expr_node_s  *init;
    struct expr_node_s  *term;
    struct expr_node_s  *step;
    struct expr_node_s  *body;
    optype_t            cmptype;
    int                 is_decr;
};
struct expr_rtncall_s {
    struct expr_node_s  *rtn;
    // need linkage XXX
    exprseq_t           inargs;
    exprseq_t           outargs;
};
struct expr_case_s {
    struct expr_node_s  *caseindex;
    long                 lowbound, highbound;
    struct expr_node_s  *outrange;
    struct expr_node_s  **cases;  // array containing hi-low+1 elements
};

struct expr_selector_s {
    struct expr_node_s  *low;
    struct expr_node_s  *high;
    struct expr_node_s  *nextsel;
    struct expr_node_s  *action;
    int is_otherwise;
    int is_always;
};
struct expr_sel_s {
    struct expr_node_s  *selindex;
    optype_t            cmptype;
    int                 selectone;
    exprseq_t           sequence;
};

struct expr_seg_s {
    name_t          *name;
    seg_t           *base_seg;
    long            offset;
    unsigned int    units;
    int             signext;
};

struct expr_exit_s {
    struct expr_node_s  *exitval;
    name_t              *exitlabel;
};

#define EXPR_M_HASVAL (1<<0)
#define EXPR_M_CTCE   (1<<1)
#define EXPR_M_LTCE   (1<<2)
struct expr_node_s {
    // for freelist tracking and sequences in blocks
    struct expr_node_s *next;
    exprtype_t          type;
    textpos_t           textpos;
    unsigned int        flags;
    union {
        struct expr_lit_s litdata;
        struct expr_seg_s  segdata;
        struct expr_struref_s  srdata;
        struct expr_blk_s  blkdata;
        struct expr_fldref_s flddata;
        struct expr_oper_s opdata;
        struct expr_func_s fdata;
        struct expr_cond_s cdata;
        struct expr_loopwu_s wudata;
        struct expr_loopid_s iddata;
        struct expr_rtncall_s rcdata;
        struct expr_case_s  casedata;
        struct expr_sel_s   seldata;
        struct expr_selector_s slctrdata;
        struct expr_exit_s  exitdata;
    } data;
};
typedef struct expr_node_s expr_node_t;

typedef expr_node_t *(*expr_dispatch_fn)(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);

const char *exprtype_name(exprtype_t type);
expr_node_t *expr_node_alloc(expr_ctx_t ctx, exprtype_t type, textpos_t pos);
void expr_node_free(expr_ctx_t ctx, expr_node_t *node);
expr_node_t *expr_node_copy(expr_ctx_t ctx, expr_node_t *src);
const char *oper_name(optype_t op);

static inline __unused int expr_is_noop(expr_node_t *node) {
    return (node == 0 ? 1 : node->type == EXPTYPE_NOOP);
}
static inline __unused int expr_is_primary(expr_node_t *node) {
    return (node == 0 ? 0 :
            (node->type >= EXPTYPE_PRIM_LIT && node->type <= EXPTYPE_PRIM_BLK));
}
static inline __unused int expr_is_opexp(expr_node_t *node) {
    return (node == 0 ? 0 : node->type == EXPTYPE_OPERATOR);
}
static inline __unused int expr_is_execfun(expr_node_t *node) {
    return (node == 0 ? 0 : node->type == EXPTYPE_EXECFUN);
}
static inline __unused int expr_is_ctrl(expr_node_t *node) {
    return (node == 0 ? 0 :
            (node->type >= EXPTYPE_CTRL_COND && node->type <= EXPTYPE_CTRL_LOOPID));
}

// Common fields
static inline __unused expr_node_t *expr_next(expr_node_t *node) {
    return node->next;
}
static inline __unused void expr_next_set(expr_node_t *node, expr_node_t *next) {
    node->next = next;
}
static inline __unused exprtype_t expr_type(expr_node_t *node) {
    return node->type;
}
static inline __unused void expr_type_set(expr_node_t *node, exprtype_t type) {
    node->type = type;
}
static inline __unused void *expr_data(expr_node_t *node) {
    return &node->data;
}
static inline __unused textpos_t expr_textpos(expr_node_t *node) {
    return node->textpos;
}
static inline __unused void expr_textpos_set(expr_node_t *node, textpos_t pos) {
    node->textpos = pos;
}
static inline __unused int expr_has_value(expr_node_t *node) {
    return (node->flags & EXPR_M_HASVAL) != 0;
}
static inline __unused void expr_has_value_set(expr_node_t *node, int v) {
    node->flags = (v ? (node->flags|EXPR_M_HASVAL) : (node->flags & ~EXPR_M_HASVAL));
}
static inline __unused int expr_is_ctce(expr_node_t *node) {
    return (node->flags & EXPR_M_CTCE) != 0;
}
static inline __unused void expr_is_ctce_set(expr_node_t *node, int v) {
    node->flags = (v ? (node->flags|EXPR_M_CTCE) : (node->flags & ~EXPR_M_CTCE));
}
static inline __unused int expr_is_ltce(expr_node_t *node) {
    return (node->flags & EXPR_M_LTCE) != 0;
}
static inline __unused void expr_is_ltce_set(expr_node_t *node, int v) {
    node->flags = (v ? (node->flags|EXPR_M_LTCE) : (node->flags & ~EXPR_M_LTCE));
}

// PRIM_LIT
static inline __unused long expr_litval(expr_node_t *node) {
    return node->data.litdata.litval;
}
static inline __unused void expr_litval_set(expr_node_t *node, long value) {
    node->data.litdata.litval = value;
}
static inline __unused strdesc_t *expr_litstring(expr_node_t *node) {
    return node->data.litdata.litstring;
}
static inline __unused void expr_litstring_set(expr_node_t *node, strdesc_t *str) {
    node->data.litdata.litstring = str;
}

// PRIM_STRUREF
static inline __unused expr_node_t *expr_struref_accexpr(expr_node_t *node) {
    return node->data.srdata.accexpr;
}
static inline __unused void expr_struref_accexpr_set(expr_node_t *node, expr_node_t *e) {
    node->data.srdata.accexpr = e;
}
// PRIM_SEG
static inline __unused seg_t *expr_seg_base(expr_node_t *node) {
    return node->data.segdata.base_seg;
}
static inline __unused void expr_seg_base_set(expr_node_t *node, seg_t *segval) {
    node->data.segdata.base_seg = segval;
}
static inline __unused unsigned int expr_seg_units(expr_node_t *node) {
    return node->data.segdata.units;
}
static inline __unused void expr_seg_units_set(expr_node_t *node, unsigned int u) {
    node->data.segdata.units = u;
}
static inline __unused int expr_seg_signext(expr_node_t *node) {
    return node->data.segdata.signext;
}
static inline __unused void expr_seg_signext_set(expr_node_t *node, int s) {
    node->data.segdata.signext = s;
}
static inline __unused long expr_seg_offset(expr_node_t *node) {
    return node->data.segdata.offset;
}
static inline __unused void expr_seg_offset_set(expr_node_t *node, long offset) {
    node->data.segdata.offset = offset;
}
// PRIM_SEGNAME
static inline __unused name_t *expr_seg_name(expr_node_t *node) {
    return node->data.segdata.name;
}
static inline __unused void expr_seg_name_set(expr_node_t *node, name_t *np) {
    node->data.segdata.name = np;
}
// PRIM_FLDREF
static inline __unused expr_node_t *expr_fldref_addr(expr_node_t *node) {
    return node->data.flddata.addr;
}
static inline __unused void expr_fldref_addr_set(expr_node_t *node, expr_node_t *a) {
    node->data.flddata.addr = a;
}
static inline __unused expr_node_t *expr_fldref_pos(expr_node_t *node) {
    return node->data.flddata.pos;
}
static inline __unused void expr_fldref_pos_set(expr_node_t *node, expr_node_t *p) {
    node->data.flddata.pos = p;
}
static inline __unused expr_node_t *expr_fldref_size(expr_node_t *node) {
    return node->data.flddata.size;
}
static inline __unused void expr_fldref_size_set(expr_node_t *node, expr_node_t *s) {
    node->data.flddata.size = s;
}
static inline __unused int expr_fldref_signext(expr_node_t *node) {
    return node->data.flddata.signext;
}
static inline __unused void expr_fldref_signext_set(expr_node_t *node, int val) {
    node->data.flddata.signext = val;
}

// PRIM_BLK
static inline __unused exprseq_t *expr_blk_seq(expr_node_t *node) {
    return &node->data.blkdata.blkseq;
}
static inline __unused expr_node_t *expr_blk_valexp(expr_node_t *node) {
    return node->data.blkdata.blkval;
}
static inline __unused void expr_blk_valexp_set(expr_node_t *node, expr_node_t *val) {
    node->data.blkdata.blkval = val;
}
static inline __unused scopectx_t expr_blk_scope(expr_node_t *node) {
    return node->data.blkdata.blkscope;
}
static inline __unused void expr_blk_scope_set(expr_node_t *node, scopectx_t scope) {
    node->data.blkdata.blkscope = scope;
}
static inline __unused strdesc_t *expr_blk_codecomment(expr_node_t *node) {
    return node->data.blkdata.codecomment;
}
static inline __unused void expr_blk_codecomment_set(expr_node_t *node, strdesc_t *str) {
    node->data.blkdata.codecomment = str;
}

// PRIM_RTNCALL
static inline __unused expr_node_t *expr_rtnaddr(expr_node_t *node) {
    return node->data.rcdata.rtn;
}
static inline __unused void expr_rtnaddr_set(expr_node_t *node, expr_node_t *adr) {
    node->data.rcdata.rtn = adr;
}
static inline __unused exprseq_t *expr_rtn_inargs(expr_node_t *node) {
    return &node->data.rcdata.inargs;
}
static inline __unused exprseq_t *expr_rtn_outargs(expr_node_t *node) {
    return &node->data.rcdata.outargs;
}
// OPERATOR
static inline __unused expr_node_t *expr_op_lhs(expr_node_t *node) {
    return node->data.opdata.op_lhs;
}
static inline __unused void expr_op_lhs_set(expr_node_t *node, expr_node_t *child) {
    node->data.opdata.op_lhs = child;
}
static inline __unused expr_node_t *expr_op_rhs(expr_node_t *node) {
    return node->data.opdata.op_rhs;
}
static inline __unused void expr_op_rhs_set(expr_node_t *node, expr_node_t *child) {
    node->data.opdata.op_rhs = child;
}
static inline __unused optype_t expr_op_type(expr_node_t *node) {
    return (node->type == EXPTYPE_OPERATOR ? node->data.opdata.op_type : OPER_NONE);
}
static inline __unused void expr_op_type_set(expr_node_t *node, optype_t op) {
    node->data.opdata.op_type = op;
}
// CTRL_COND
static inline __unused expr_node_t *expr_cond_test(expr_node_t *node) {
    return node->data.cdata.test;
}
static inline __unused void expr_cond_test_set(expr_node_t *node, expr_node_t *test) {
    node->data.cdata.test = test;
}
static inline __unused expr_node_t *expr_cond_consequent(expr_node_t *node) {
    return node->data.cdata.cons;
}
static inline __unused void expr_cond_consequent_set(expr_node_t *node, expr_node_t *cons) {
    node->data.cdata.cons = cons;
}
static inline __unused expr_node_t *expr_cond_alternative(expr_node_t *node) {
    return node->data.cdata.alt;
}
static inline __unused void expr_cond_alternative_set(expr_node_t *node, expr_node_t *alt) {
    node->data.cdata.alt = alt;
}
// CTRL_WULOOP
static inline __unused expr_node_t *expr_wuloop_test(expr_node_t *node) {
    return node->data.wudata.test;
}
static inline __unused void expr_wuloop_test_set(expr_node_t *node, expr_node_t *test) {
    node->data.wudata.test = test;
}
static inline __unused expr_node_t *expr_wuloop_body(expr_node_t *node) {
   return node->data.wudata.body;
}
static inline __unused void expr_wuloop_body_set(expr_node_t *node, expr_node_t *body) {
    node->data.wudata.body = body;
}
static inline __unused looptesttype_t expr_wuloop_type(expr_node_t *node) {
    return node->data.wudata.type;
}
static inline __unused void expr_wuloop_type_set(expr_node_t *node,
                                                 looptesttype_t type) {
    node->data.wudata.type = type;
}
// CTRL_IDLOOP
static inline __unused scopectx_t expr_idloop_scope(expr_node_t *node) {
    return node->data.iddata.scope;
}
static inline __unused void expr_idloop_scope_set(expr_node_t *node, scopectx_t scope) {
    node->data.iddata.scope = scope;
}
static inline __unused expr_node_t *expr_idloop_init(expr_node_t *node) {
    return node->data.iddata.init;
}
static inline __unused void expr_idloop_init_set(expr_node_t *node, expr_node_t *init) {
    node->data.iddata.init = init;
}
static inline __unused expr_node_t *expr_idloop_term(expr_node_t *node) {
    return node->data.iddata.term;
}
static inline __unused void expr_idloop_term_set(expr_node_t *node, expr_node_t *term) {
    node->data.iddata.term = term;
}
static inline __unused expr_node_t *expr_idloop_step(expr_node_t *node) {
    return node->data.iddata.step;
}
static inline __unused void expr_idloop_step_set(expr_node_t *node, expr_node_t *step) {
    node->data.iddata.step = step;
}
static inline __unused expr_node_t *expr_idloop_body(expr_node_t *node) {
    return node->data.iddata.body;
}
static inline __unused void expr_idloop_body_set(expr_node_t *node, expr_node_t *body) {
    node->data.iddata.body = body;
}
static inline __unused optype_t expr_idloop_cmptype(expr_node_t *node) {
    return node->data.iddata.cmptype;
}
static inline __unused void expr_idloop_cmptype_set(expr_node_t *node, optype_t cmp) {
    node->data.iddata.cmptype = cmp;
}
static inline __unused int expr_idloop_decr(expr_node_t *node) {
    return node->data.iddata.is_decr;
}
static inline __unused void expr_idloop_decr_set(expr_node_t *node, int val) {
    node->data.iddata.is_decr = val;
}
// CTRL_CASE
static inline __unused expr_node_t *expr_case_index(expr_node_t *node) {
    return node->data.casedata.caseindex;
}
static inline __unused void expr_case_index_set(expr_node_t *node, expr_node_t *exp) {
    node->data.casedata.caseindex = exp;
}
static inline __unused long expr_case_lowbound(expr_node_t *node) {
    return node->data.casedata.lowbound;
}
static inline __unused long expr_case_highbound(expr_node_t *node) {
    return node->data.casedata.highbound;
}
static inline __unused void expr_case_bounds_set(expr_node_t *node, long lo, long hi) {
    node->data.casedata.highbound = hi;
    node->data.casedata.lowbound = lo;
}
static inline __unused expr_node_t *expr_case_outrange(expr_node_t *node) {
    return node->data.casedata.outrange;
}
static inline __unused void expr_case_outrange_set(expr_node_t *node, expr_node_t *exp) {
    node->data.casedata.outrange = exp;
}
static inline __unused expr_node_t *expr_case_action(expr_node_t *node, long which) {
    if (which < node->data.casedata.lowbound || which > node->data.casedata.highbound) {
        return node->data.casedata.outrange;
    }
    return node->data.casedata.cases[which-node->data.casedata.lowbound];
}
static inline __unused expr_node_t **expr_case_cases(expr_node_t *node) {
    return node->data.casedata.cases;
}
static inline __unused void expr_case_actions_set(expr_node_t *node, expr_node_t **arr) {
    node->data.casedata.cases = arr;
}
// CTRL_SELECT
static inline __unused expr_node_t *expr_sel_index(expr_node_t *node) {
    return node->data.seldata.selindex;
}
static inline __unused void expr_sel_index_set(expr_node_t *node, expr_node_t *si) {
    node->data.seldata.selindex = si;
}
static inline __unused optype_t expr_sel_cmptype(expr_node_t *node) {
    return node->data.seldata.cmptype;
}
static inline __unused void expr_sel_cmptype_set(expr_node_t *node, optype_t op) {
    node->data.seldata.cmptype = op;
}
static inline __unused int expr_sel_oneonly(expr_node_t *node) {
    return node->data.seldata.selectone;
}
static inline __unused void expr_sel_oneonly_set(expr_node_t *node, int val) {
    node->data.seldata.selectone = val;
}
static inline __unused exprseq_t *expr_sel_selectors(expr_node_t *node) {
    return &node->data.seldata.sequence;
}
// selectors
static inline __unused expr_node_t *expr_selector_low(expr_node_t *node) {
    return node->data.slctrdata.low;
}
static inline __unused expr_node_t *expr_selector_high(expr_node_t *node) {
    return node->data.slctrdata.high;
}
static inline __unused void expr_selector_lohi_set(expr_node_t *node,
                                                   expr_node_t *lo, expr_node_t *hi) {
    node->data.slctrdata.low = lo;
    node->data.slctrdata.high = hi;
}
static inline __unused expr_node_t *expr_selector_action(expr_node_t *node) {
    return node->data.slctrdata.action;
}
static inline __unused void expr_selector_action_set(expr_node_t *node, expr_node_t *act) {
    node->data.slctrdata.action = act;
}
static inline __unused int expr_selector_otherwise(expr_node_t *node) {
    return node->data.slctrdata.is_otherwise;
}
static inline __unused void expr_selector_otherwise_set(expr_node_t *node, int val) {
    node->data.slctrdata.is_otherwise = val;
}
static inline __unused int expr_selector_always(expr_node_t *node) {
    return node->data.slctrdata.is_always;
}
static inline __unused void expr_selector_always_set(expr_node_t *node, int val) {
    node->data.slctrdata.is_always = val;
}
static inline __unused expr_node_t *expr_selector_next(expr_node_t *node) {
    return node->data.slctrdata.nextsel;
}
static inline __unused void expr_selector_next_set(expr_node_t *node, expr_node_t *sel) {
    node->data.slctrdata.nextsel = sel;
}
// Exit expressions (EXITLOOP, LEAVE)
static inline __unused expr_node_t *expr_exit_value(expr_node_t *node) {
    return node->data.exitdata.exitval;
}
static inline __unused void expr_exit_value_set(expr_node_t *node, expr_node_t *val) {
    node->data.exitdata.exitval = val;
}
static inline __unused name_t *expr_exit_label(expr_node_t *node) {
    return node->data.exitdata.exitlabel;
}
static inline __unused void expr_exit_label_set(expr_node_t *node, name_t *np) {
    node->data.exitdata.exitlabel = np;
}

static inline __unused void exprseq_init (exprseq_t *seq) {
    seq->head = seq->tail = 0; seq->count = 0;
}
static inline __unused int exprseq_empty (exprseq_t *seq) {
    return (seq->count == 0);
}
static inline __unused void exprseq_inshead (exprseq_t *seq, expr_node_t *l) {
    if (seq->count == 0) seq->tail = l;
    l->next = seq->head; seq->head = l; seq->count += 1;
}
static inline __unused void exprseq_instail (exprseq_t *seq, expr_node_t *l) {
    if (seq->count == 0) exprseq_inshead(seq, l);
    else { seq->tail->next = l; l->next = 0; seq->tail = l; seq->count += 1;}
}
static inline __unused void exprseq_append (exprseq_t *dst, exprseq_t *addon) {
    if (addon->count == 0) return;
    if (dst->count == 0) {
        dst->head = addon->head; dst->tail = addon->tail; dst->count = addon->count;
    } else {
        dst->tail->next = addon->head; dst->tail = addon->tail;
        dst->count += addon->count;
    }
    addon->head = addon->tail = 0; addon->count = 0;
}
static inline __unused void exprseq_prepend (exprseq_t *dst, exprseq_t *addon) {
    if (addon->count == 0) return;
    if (dst->count == 0) {
        dst->head = addon->head; dst->tail = addon->tail; dst->count = addon->count;
    } else {
        addon->tail->next = dst->head; dst->head = addon->head;
        dst->count += addon->count;
    }
    addon->head = addon->tail = 0; addon->count = 0;
}
static inline __unused expr_node_t *exprseq_remhead (exprseq_t *seq) {
    expr_node_t *l = seq->head;
    if (l == 0) return l;
    seq->head = l->next;
    l->next = 0;
    seq->count -= 1;
    return l;
}
static inline __unused expr_node_t *exprseq_remtail (exprseq_t *seq) {
    expr_node_t *l = seq->tail, *p = seq->head;
    if (p == 0) return 0;
    if (seq->count == 1) {
        l = seq->head;
        seq->head = seq->tail = 0;
        seq->count = 0;
        return l;
    }
    while (p->next != l) { p = p->next; }
    p->next = 0;
    seq->tail = p;
    seq->count -= 1;
    return l;
}
static inline __unused expr_node_t *exprseq_head (exprseq_t *seq) {
    return seq->head;
}
static inline __unused expr_node_t *exprseq_tail (exprseq_t *seq) {
    return seq->tail;
}
static inline __unused int exprseq_length (exprseq_t *seq) {
    return seq->count;
}
static inline __unused void exprseq_set (exprseq_t *dst, exprseq_t *src) {
    dst->head = src->head; dst->tail = src->tail; dst->count = src->count;
}

void exprseq_free(expr_ctx_t ctx, exprseq_t *seq);
void exprseq_copy(expr_ctx_t ctx, exprseq_t *dst, exprseq_t *src);

expr_ctx_t expr_init (parse_ctx_t pctx, stgctx_t stg, scopectx_t kwdscope);
int expr_expr_next(expr_ctx_t ctx, expr_node_t **expp);
int expr_parse_ctce(expr_ctx_t ctx, lexeme_t **lex);
int expr_parse_block(expr_ctx_t ctx, expr_node_t **blkexp);
int expr_parse_seq(expr_ctx_t ctx, lexseq_t *seq, expr_node_t **expp);
void expr_dispatch_register(expr_ctx_t ctx, lextype_t lt, expr_dispatch_fn fn);
parse_ctx_t expr_parse_ctx(expr_ctx_t ctx);
void *expr_decl_ctx(expr_ctx_t ctx);
void expr_decl_ctx_set(expr_ctx_t ctx, void *d);
stgctx_t expr_stg_ctx(expr_ctx_t);
machinedef_t *expr_machinedef(expr_ctx_t);
expr_node_t *expr_parse_arglist(expr_ctx_t ctx, expr_node_t *rtn);
initval_t *expr_initval_add(expr_ctx_t ctx, initval_t *ivlist, expr_node_t *exp,
                            unsigned int width);
int expr_parse_ISSTRING(expr_ctx_t ctx, int *allstr);
int expr_parse_xCTE(expr_ctx_t ctx, int checkltce, int *allok);
int expr_get_allocation(expr_ctx_t ctx, strdesc_t *name, unsigned int *units);
int expr_parse_SIZE(expr_ctx_t ctx, unsigned int *units);
#endif
