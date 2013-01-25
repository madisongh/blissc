#ifndef expression_h__
#define expression_h__
/*
 *++
 *	File:			expression.h
 *
 *	Abstract:		Expression definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include "parser.h"
#include "nametable.h"
#include "lexeme.h"
#include "support/logging.h"
#include "support/utils.h"

// Factory macros for expression types
#undef DOEXPTYPE
#define DOEXPTYPES \
    DOEXPTYPE(NOOP) \
    DOEXPTYPE(PRIM_LIT) DOEXPTYPE(PRIM_SEG) \
    DOEXPTYPE(PRIM_FLDREF) DOEXPTYPE(PRIM_RTNCALL) \
    DOEXPTYPE(PRIM_STRUREF) \
    DOEXPTYPE(PRIM_BLK) DOEXPTYPE(OPERATOR) \
    DOEXPTYPE(EXECFUN) DOEXPTYPE(CTRL_COND) \
    DOEXPTYPE(CTRL_CASE) DOEXPTYPE(CTRL_EXIT) \
    DOEXPTYPE(CTRL_RET) \
    DOEXPTYPE(CTRL_SELECT) DOEXPTYPE(SELECTOR) \
    DOEXPTYPE(CTRL_LOOPWU) DOEXPTYPE(CTRL_LOOPID)
#define DOEXPTYPE(t_) EXPTYPE_##t_,
typedef enum {
    DOEXPTYPES
} exprtype_t;
#undef DOEXPTYPE
#define EXPTYPE_COUNT (EXPTYPE_CTRL_LOOPID+1)

// Factory macros for operator types
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

// Expression context is private
struct expr_ctx_s;
typedef struct expr_ctx_s *expr_ctx_t;

// Expression node structure is exposed here
// XXX - should this go private?
struct expr_node_s;

struct exprseq_s {
	TQ_HDR_FIELDS		(struct expr_node_s)
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
    namereflist_t       labels;
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
    name_t             *funcname;
    exprseq_t           arglist;
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
    name_t              *loopindex;
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
    long                 lowbound, highbound, actioncount;
    long                 outrange; // index into 'actions' for outrange case
    long                *cases;   // array containing hi-low+1 indices into 'actions'
    struct expr_node_s  **actions;  // array containing action node pointers
};

struct expr_selector_s {
    struct expr_node_s  *low;
    struct expr_node_s  *high;
    struct expr_node_s  *nextsel;
    struct expr_node_s  *action;
};
struct expr_sel_s {
    struct expr_node_s  *selindex;
    struct expr_node_s  *alwaysaction;
    struct expr_node_s  *otherwiseaction;
    optype_t            cmptype;
    int                 selectone;
    exprseq_t           sequence;
};

struct expr_seg_s {
    name_t          *name;
    long            offset;
    unsigned int    width;
    int             signext;
};

struct expr_exit_s { // also used for RETURN
    struct expr_node_s  *exitval;
    name_t              *exitlabel;
};

#define EXPR_M_HASVAL (1<<0)
#define EXPR_M_CTCE   (1<<1)
#define EXPR_M_LTCE   (1<<2)
struct expr_node_s {
	TQ_ENT_FIELDS(struct expr_node_s)
    void               *genref;
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
int oper_is_unary(optype_t op);

/*
 * Getters/setters for the various expression objects
 * NB: Use these instead of directly referencing the fields!
 */
#undef siu
#define siu static inline __attribute__((unused))
siu int expr_is_noop(expr_node_t *node) {
	return (node == 0 ? 1 : node->type == EXPTYPE_NOOP); }
siu int expr_is_primary(expr_node_t *node) {
    return (node == 0 ? 0 :
            (node->type >= EXPTYPE_PRIM_LIT && node->type <= EXPTYPE_PRIM_BLK)); }
siu int expr_is_opexp(expr_node_t *node) {
    return (node == 0 ? 0 : node->type == EXPTYPE_OPERATOR); }
siu int expr_is_execfun(expr_node_t *node) {
    return (node == 0 ? 0 : node->type == EXPTYPE_EXECFUN); }
siu int expr_is_ctrl(expr_node_t *node) {
    return (node == 0 ? 0 :
            (node->type >= EXPTYPE_CTRL_COND && node->type <= EXPTYPE_CTRL_LOOPID)); }

// Common fields
siu expr_node_t *expr_next(expr_node_t *node) { return node->tq_next; }
siu void expr_next_set(expr_node_t *node, expr_node_t *next) { node->tq_next = next; }
siu void *expr_genref(expr_node_t *node) { return node->genref; }
siu void expr_genref_set(expr_node_t *node, void *ref) { node->genref = ref; }
siu exprtype_t expr_type(expr_node_t *node) { return node->type; }
siu void expr_type_set(expr_node_t *node, exprtype_t type) { node->type = type; }
siu void *expr_data(expr_node_t *node) { return &node->data; }
siu textpos_t expr_textpos(expr_node_t *node) { return node->textpos; }
siu void expr_textpos_set(expr_node_t *node, textpos_t pos) { node->textpos = pos; }
siu int expr_has_value(expr_node_t *node) { return (node->flags & EXPR_M_HASVAL) != 0; }
siu void expr_has_value_set(expr_node_t *node, int v) {
    node->flags = (v ? (node->flags|EXPR_M_HASVAL) : (node->flags & ~EXPR_M_HASVAL)); }
siu int expr_is_ctce(expr_node_t *node) {
    return node == 0 || (node->flags & EXPR_M_CTCE) != 0; }
siu void expr_is_ctce_set(expr_node_t *node, int v) {
    node->flags = (v ? (node->flags|EXPR_M_CTCE) : (node->flags & ~EXPR_M_CTCE)); }
siu int expr_is_ltce(expr_node_t *node) {
    return (node->flags & (EXPR_M_CTCE|EXPR_M_LTCE)) != 0; }
siu int expr_is_ltce_only(expr_node_t *node) {
    return (node->flags & EXPR_M_LTCE) != 0; }
siu void expr_is_ltce_set(expr_node_t *node, int v) { if (node == 0) return;
    node->flags = (v ? (node->flags|EXPR_M_LTCE) : (node->flags & ~EXPR_M_LTCE)); }

// PRIM_LIT
siu long expr_litval(expr_node_t *node) { return node->data.litdata.litval; }
siu void expr_litval_set(expr_node_t *node, long value) {
	node->data.litdata.litval = value; }
siu strdesc_t *expr_litstring(expr_node_t *node) { return node->data.litdata.litstring; }
siu void expr_litstring_set(expr_node_t *node, strdesc_t *str) {
	node->data.litdata.litstring = str; }

// PRIM_STRUREF
siu expr_node_t *expr_struref_accexpr(expr_node_t *node) {
    return node->data.srdata.accexpr; }
siu void expr_struref_accexpr_set(expr_node_t *node, expr_node_t *e) {
    node->data.srdata.accexpr = e; }

// PRIM_SEG
siu unsigned int expr_seg_width(expr_node_t *node) { return node->data.segdata.width; }
siu void expr_seg_width_set(expr_node_t *node, unsigned int u) {
    node->data.segdata.width = u; }
siu int expr_seg_signext(expr_node_t *node) { return node->data.segdata.signext; }
siu void expr_seg_signext_set(expr_node_t *node, int s) {
	node->data.segdata.signext = s; }
siu long expr_seg_offset(expr_node_t *node) { return node->data.segdata.offset; }
siu void expr_seg_offset_set(expr_node_t *node, long offset) {
    node->data.segdata.offset = offset; }
siu name_t *expr_seg_name(expr_node_t *node) { return node->data.segdata.name; }
siu void expr_seg_name_set(expr_node_t *node, name_t *np) {
	node->data.segdata.name = np; }

// PRIM_FLDREF
siu expr_node_t *expr_fldref_addr(expr_node_t *node) { return node->data.flddata.addr; }
siu void expr_fldref_addr_set(expr_node_t *node, expr_node_t *a) {
    node->data.flddata.addr = a; }
siu expr_node_t *expr_fldref_pos(expr_node_t *node) { return node->data.flddata.pos; }
siu void expr_fldref_pos_set(expr_node_t *node, expr_node_t *p) {
    node->data.flddata.pos = p; }
siu expr_node_t *expr_fldref_size(expr_node_t *node) { return node->data.flddata.size; }
siu void expr_fldref_size_set(expr_node_t *node, expr_node_t *s) {
    node->data.flddata.size = s; }
siu int expr_fldref_signext(expr_node_t *node) { return node->data.flddata.signext; }
siu void expr_fldref_signext_set(expr_node_t *node, int val) {
    node->data.flddata.signext = val; }

// PRIM_BLK
siu exprseq_t *expr_blk_seq(expr_node_t *node) { return &node->data.blkdata.blkseq; }
siu namereflist_t *expr_blk_labels(expr_node_t *node) {
    return &node->data.blkdata.labels; }
siu expr_node_t *expr_blk_valexp(expr_node_t *node) { return node->data.blkdata.blkval; }
siu void expr_blk_valexp_set(expr_node_t *node, expr_node_t *val) {
    node->data.blkdata.blkval = val; }
siu scopectx_t expr_blk_scope(expr_node_t *node) {
    return node->data.blkdata.blkscope; }
siu void expr_blk_scope_set(expr_node_t *node, scopectx_t scope) {
    node->data.blkdata.blkscope = scope; }
siu strdesc_t *expr_blk_codecomment(expr_node_t *node) {
    return node->data.blkdata.codecomment; }
siu void expr_blk_codecomment_set(expr_node_t *node, strdesc_t *str) {
    node->data.blkdata.codecomment = str; }

// PRIM_RTNCALL
siu expr_node_t *expr_rtnaddr(expr_node_t *node) { return node->data.rcdata.rtn; }
siu void expr_rtnaddr_set(expr_node_t *node, expr_node_t *adr) {
    node->data.rcdata.rtn = adr; }
siu exprseq_t *expr_rtn_inargs(expr_node_t *node) {
    return &node->data.rcdata.inargs; }
siu exprseq_t *expr_rtn_outargs(expr_node_t *node) {
    return &node->data.rcdata.outargs; }

// EXECFUN
siu name_t *expr_func_name(expr_node_t *node) { return node->data.fdata.funcname; }
siu void expr_func_name_set(expr_node_t *node, name_t *name) {
    node->data.fdata.funcname = name; }
siu exprseq_t *expr_func_arglist(expr_node_t *node) {
    return &node->data.fdata.arglist; }

// OPERATOR
siu expr_node_t *expr_op_lhs(expr_node_t *node) { return node->data.opdata.op_lhs; }
siu void expr_op_lhs_set(expr_node_t *node, expr_node_t *child) {
    node->data.opdata.op_lhs = child; }
siu expr_node_t *expr_op_rhs(expr_node_t *node) { return node->data.opdata.op_rhs; }
siu void expr_op_rhs_set(expr_node_t *node, expr_node_t *child) {
    node->data.opdata.op_rhs = child; }
siu optype_t expr_op_type(expr_node_t *node) {
    return (node->type == EXPTYPE_OPERATOR ? node->data.opdata.op_type : OPER_NONE); }
siu void expr_op_type_set(expr_node_t *node, optype_t op) {
    node->data.opdata.op_type = op; }

// CTRL_COND
siu expr_node_t *expr_cond_test(expr_node_t *node) { return node->data.cdata.test; }
siu void expr_cond_test_set(expr_node_t *node, expr_node_t *test) {
    node->data.cdata.test = test; }
siu expr_node_t *expr_cond_consequent(expr_node_t *node) {
    return node->data.cdata.cons; }
siu void expr_cond_consequent_set(expr_node_t *node, expr_node_t *cons) {
    node->data.cdata.cons = cons; }
siu expr_node_t *expr_cond_alternative(expr_node_t *node) {
    return node->data.cdata.alt; }
siu void expr_cond_alternative_set(expr_node_t *node, expr_node_t *alt) {
    node->data.cdata.alt = alt; }

// CTRL_WULOOP
siu expr_node_t *expr_wuloop_test(expr_node_t *node) { return node->data.wudata.test; }
siu void expr_wuloop_test_set(expr_node_t *node, expr_node_t *test) {
    node->data.wudata.test = test; }
siu expr_node_t *expr_wuloop_body(expr_node_t *node) { return node->data.wudata.body; }
siu void expr_wuloop_body_set(expr_node_t *node, expr_node_t *body) {
    node->data.wudata.body = body; }
siu looptesttype_t expr_wuloop_type(expr_node_t *node) { return node->data.wudata.type; }
siu void expr_wuloop_type_set(expr_node_t *node, looptesttype_t type) {
    node->data.wudata.type = type; }

// CTRL_IDLOOP
siu scopectx_t expr_idloop_scope(expr_node_t *node) { return node->data.iddata.scope; }
siu void expr_idloop_scope_set(expr_node_t *node, scopectx_t scope) {
    node->data.iddata.scope = scope; }
siu name_t *expr_idloop_index(expr_node_t *node) { return node->data.iddata.loopindex; }
siu void expr_idloop_index_set(expr_node_t *node, name_t *v) {
    node->data.iddata.loopindex = v; }
siu expr_node_t *expr_idloop_init(expr_node_t *node) { return node->data.iddata.init; }
siu void expr_idloop_init_set(expr_node_t *node, expr_node_t *init) {
    node->data.iddata.init = init; }
siu expr_node_t *expr_idloop_term(expr_node_t *node) { return node->data.iddata.term; }
siu void expr_idloop_term_set(expr_node_t *node, expr_node_t *term) {
    node->data.iddata.term = term; }
siu expr_node_t *expr_idloop_step(expr_node_t *node) { return node->data.iddata.step; }
siu void expr_idloop_step_set(expr_node_t *node, expr_node_t *step) {
    node->data.iddata.step = step; }
siu expr_node_t *expr_idloop_body(expr_node_t *node) { return node->data.iddata.body; }
siu void expr_idloop_body_set(expr_node_t *node, expr_node_t *body) {
    node->data.iddata.body = body; }
siu optype_t expr_idloop_cmptype(expr_node_t *node) {
    return node->data.iddata.cmptype; }
siu void expr_idloop_cmptype_set(expr_node_t *node, optype_t cmp) {
    node->data.iddata.cmptype = cmp; }
siu int expr_idloop_decr(expr_node_t *node) {
    return node->data.iddata.is_decr; }
siu void expr_idloop_decr_set(expr_node_t *node, int val) {
    node->data.iddata.is_decr = val; }

// CTRL_CASE
siu expr_node_t *expr_case_index(expr_node_t *node) {
	return node->data.casedata.caseindex; }
siu void expr_case_index_set(expr_node_t *node, expr_node_t *exp) {
    node->data.casedata.caseindex = exp; }
siu long expr_case_lowbound(expr_node_t *node) {
    return node->data.casedata.lowbound; }
siu long expr_case_highbound(expr_node_t *node) {
    return node->data.casedata.highbound; }
siu void expr_case_bounds_set(expr_node_t *node, long lo, long hi) {
    node->data.casedata.highbound = hi;
    node->data.casedata.lowbound = lo; }
siu long expr_case_outrange(expr_node_t *node) {
    return node->data.casedata.outrange; }
siu void expr_case_outrange_set(expr_node_t *node, long val) {
    node->data.casedata.outrange = val; }
siu expr_node_t *expr_case_action(expr_node_t *node, long which) {
    struct expr_case_s *c = &node->data.casedata;
    if (which < c->lowbound || which > c->highbound)
        return (c->outrange < 0 ? 0 : c->actions[c->outrange]);
    else return c->actions[c->cases[which-c->lowbound]]; }
siu long *expr_case_cases(expr_node_t *node) {
    return node->data.casedata.cases; }
siu void expr_case_cases_set(expr_node_t *node, long *cases) {
    node->data.casedata.cases = cases; }
siu void expr_case_actions_set(expr_node_t *node, long count, expr_node_t **arr) {
    node->data.casedata.actions = arr; node->data.casedata.actioncount = count; }
siu long expr_case_actioncount(expr_node_t *node) {
    return node->data.casedata.actioncount; }
siu expr_node_t **expr_case_actions(expr_node_t *node) {
    return node->data.casedata.actions;
}

// CTRL_SELECT
siu expr_node_t *expr_sel_index(expr_node_t *node) { return node->data.seldata.selindex; }
siu void expr_sel_index_set(expr_node_t *node, expr_node_t *si) {
    node->data.seldata.selindex = si; }
siu optype_t expr_sel_cmptype(expr_node_t *node) { return node->data.seldata.cmptype; }
siu void expr_sel_cmptype_set(expr_node_t *node, optype_t op) {
    node->data.seldata.cmptype = op; }
siu int expr_sel_oneonly(expr_node_t *node) { return node->data.seldata.selectone; }
siu void expr_sel_oneonly_set(expr_node_t *node, int val) {
    node->data.seldata.selectone = val; }
siu exprseq_t *expr_sel_selectors(expr_node_t *node) {
	return &node->data.seldata.sequence; }
siu expr_node_t *expr_sel_alwaysaction(expr_node_t *node) {
    return node->data.seldata.alwaysaction; }
siu void expr_sel_alwaysaction_set(expr_node_t *node, expr_node_t *exp) {
    node->data.seldata.alwaysaction = exp; }
siu expr_node_t *expr_sel_otherwiseaction(expr_node_t *node) {
    return node->data.seldata.otherwiseaction; }
siu void expr_sel_otherwiseaction_set(expr_node_t *node, expr_node_t *exp) {
    node->data.seldata.otherwiseaction = exp; }
// selectors
siu expr_node_t *expr_selector_low(expr_node_t *node) { return node->data.slctrdata.low; }
siu expr_node_t *expr_selector_high(expr_node_t *node) {
	return node->data.slctrdata.high; }
siu void expr_selector_lohi_set(expr_node_t *node, expr_node_t *lo, expr_node_t *hi) {
    node->data.slctrdata.low = lo;
    node->data.slctrdata.high = hi; }
siu expr_node_t *expr_selector_action(expr_node_t *node) {
    return node->data.slctrdata.action; }
siu void expr_selector_action_set(expr_node_t *node, expr_node_t *act) {
    node->data.slctrdata.action = act; }
siu expr_node_t *expr_selector_next(expr_node_t *node) {
    return node->data.slctrdata.nextsel; }
siu void expr_selector_next_set(expr_node_t *node, expr_node_t *sel) {
    node->data.slctrdata.nextsel = sel; }

// Exit expressions (EXITLOOP, LEAVE, RETURN)
siu expr_node_t *expr_exit_value(expr_node_t *node) {
	return node->data.exitdata.exitval; }
siu void expr_exit_value_set(expr_node_t *node, expr_node_t *val) {
    node->data.exitdata.exitval = val; }
siu name_t *expr_exit_label(expr_node_t *node) {
    return node->data.exitdata.exitlabel; }
siu void expr_exit_label_set(expr_node_t *node, name_t *np) {
    node->data.exitdata.exitlabel = np; }
#undef siu

// Expression sequence functions
DEFINE_TQ_FUNCS(exprseq, exprseq_t, expr_node_t)
void exprseq_free(expr_ctx_t ctx, exprseq_t *seq);
void exprseq_copy(expr_ctx_t ctx, exprseq_t *dst, exprseq_t *src);

expr_ctx_t expr_init (strctx_t strctx, parse_ctx_t pctx, scopectx_t kwdscope);
void expr_finish(expr_ctx_t ctx);
int expr_parse_expr(expr_ctx_t ctx, expr_node_t **expp);
int expr_parse_ctce(expr_ctx_t ctx, lexeme_t **lex, long *valp);
int expr_parse_block(expr_ctx_t ctx, expr_node_t **blkexp);
int expr_parse_seq(expr_ctx_t ctx, lexseq_t *seq, expr_node_t **expp);
void expr_dispatch_register(expr_ctx_t ctx, lextype_t lt, expr_dispatch_fn fn);
parse_ctx_t expr_parse_ctx(expr_ctx_t ctx);
void *expr_decl_ctx(expr_ctx_t ctx);
void expr_decl_ctx_set(expr_ctx_t ctx, void *d);
void *expr_gencodectx(expr_ctx_t);
machinedef_t *expr_machinedef(expr_ctx_t);
namectx_t expr_namectx(expr_ctx_t);
lexctx_t expr_lexmemctx(expr_ctx_t);
logctx_t expr_logctx(expr_ctx_t);
strctx_t expr_strctx(expr_ctx_t);
lstgctx_t expr_lstgctx(expr_ctx_t);
void *expr_symctx(expr_ctx_t);
expr_node_t *expr_parse_arglist(expr_ctx_t ctx, expr_node_t *rtn);
initval_t *expr_initval_add(expr_ctx_t ctx, initval_t *ivlist, expr_node_t *exp,
                            unsigned int width);
void expr_push_routine(expr_ctx_t ctx, name_t *np);
void expr_pop_routine(expr_ctx_t ctx);
name_t *expr_current_routine (expr_ctx_t ctx);
void expr_loopdepth_incr(expr_ctx_t ctx);
void expr_loopdepth_decr(expr_ctx_t ctx);
int expr_loopdepth_get(expr_ctx_t ctx);
void *expr_fake_label_ptr(expr_ctx_t ctx);
void expr_signal(expr_ctx_t ctx, statcode_t code, ...);

#endif /* expression_h__ */
