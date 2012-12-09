//
//  expression.c
//  blissc
//
//  Created by Matthew Madison on 11/1/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "expression.h"
#include "declarations.h"
#include "symbols.h"
#include "storage.h"
#include "parser.h"
#include "nametable.h"
#include "lexeme.h"
#include "machinedef.h"

#undef DOEXPTYPE
#define DOEXPTYPE(t_) "EXPTYPE_" #t_,
static const char *exptypenames[] = { DOEXPTYPES };
#undef DOEXPTYPE

struct expr_ctx_s {
    parse_ctx_t         pctx;
    stgctx_t            stg;
    namectx_t           namectx;
    machinedef_t       *mach;
    expr_node_t        *freenodes;
    expr_dispatch_fn    dispatchers[LEXTYPE_EXPKWD_MAX-LEXTYPE_EXPKWD_MIN+1];
    expr_dispatch_fn    name_dispatchers[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    exprseq_t           exprseq;
    unsigned int        loopdepth;
    int                 longstringsok;
    int                 noreduce;
};

#define ALLOC_QTY       128

static namedef_t expr_names[] = {
    NAMEDEF("PLIT", LEXTYPE_KWD_PLIT, NAME_M_RESERVED),
    NAMEDEF("UPLIT", LEXTYPE_KWD_UPLIT, NAME_M_RESERVED),
    NAMEDEF("CODECOMMENT", LEXTYPE_KWD_CODECOMMENT, NAME_M_RESERVED),
    NAMEDEF("BEGIN", LEXTYPE_EXP_DELIM_BEGIN, NAME_M_RESERVED),
    NAMEDEF("END", LEXTYPE_EXP_DELIM_END, NAME_M_RESERVED),
    NAMEDEF("IF", LEXTYPE_CTRL_IF, NAME_M_RESERVED),
    NAMEDEF("THEN", LEXTYPE_CTRL_THEN, NAME_M_RESERVED),
    NAMEDEF("ELSE", LEXTYPE_CTRL_ELSE, NAME_M_RESERVED),
    NAMEDEF("CASE", LEXTYPE_CTRL_CASE, NAME_M_RESERVED),
    NAMEDEF("FROM", LEXTYPE_KWD_FROM, NAME_M_RESERVED),
    NAMEDEF("TO", LEXTYPE_KWD_TO, NAME_M_RESERVED),
    NAMEDEF("OF", LEXTYPE_KWD_OF, NAME_M_RESERVED),
    NAMEDEF("BY", LEXTYPE_KWD_BY, NAME_M_RESERVED),
    NAMEDEF("SET", LEXTYPE_KWD_SET, NAME_M_RESERVED),
    NAMEDEF("TES", LEXTYPE_KWD_TES, NAME_M_RESERVED),
    NAMEDEF("INRANGE", LEXTYPE_KWD_INRANGE, NAME_M_RESERVED),
    NAMEDEF("OUTRANGE", LEXTYPE_KWD_OUTRANGE, NAME_M_RESERVED),
    NAMEDEF("SELECT", LEXTYPE_CTRL_SELECT, NAME_M_RESERVED),
    NAMEDEF("SELECTU", LEXTYPE_CTRL_SELECTU, NAME_M_RESERVED),
    NAMEDEF("SELECTA", LEXTYPE_CTRL_SELECTA, NAME_M_RESERVED),
    NAMEDEF("SELECTONE", LEXTYPE_CTRL_SELECTONE, NAME_M_RESERVED),
    NAMEDEF("SELECTONEU", LEXTYPE_CTRL_SELECTONEU, NAME_M_RESERVED),
    NAMEDEF("SELECTONEA", LEXTYPE_CTRL_SELECTONEA, NAME_M_RESERVED),
    NAMEDEF("INCR", LEXTYPE_CTRL_INCR, NAME_M_RESERVED),
    NAMEDEF("INCRA", LEXTYPE_CTRL_INCRA, NAME_M_RESERVED),
    NAMEDEF("INCRU", LEXTYPE_CTRL_INCRU, NAME_M_RESERVED),
    NAMEDEF("DECR", LEXTYPE_CTRL_DECR, NAME_M_RESERVED),
    NAMEDEF("DECRA", LEXTYPE_CTRL_DECRA, NAME_M_RESERVED),
    NAMEDEF("DECRU", LEXTYPE_CTRL_DECRU, NAME_M_RESERVED),
    NAMEDEF("WHILE", LEXTYPE_CTRL_WHILE, NAME_M_RESERVED),
    NAMEDEF("UNTIL", LEXTYPE_CTRL_UNTIL, NAME_M_RESERVED),
    NAMEDEF("DO", LEXTYPE_CTRL_DO, NAME_M_RESERVED),
    NAMEDEF("EXITLOOP", LEXTYPE_CTRL_EXITLOOP, NAME_M_RESERVED),
    NAMEDEF("LEAVE", LEXTYPE_CTRL_LEAVE, NAME_M_RESERVED),
    NAMEDEF("WITH", LEXTYPE_KWD_WITH, NAME_M_RESERVED)
};

#undef OPMAP
#define OPMAPS \
    OPMAP(ADD,ADD) OPMAP(SUB,SUBTRACT) \
    OPMAP(MUL,MULT) OPMAP(DIV,DIV) \
    OPMAP(MOD,MODULO) OPMAP(ASSIGN,ASSIGN) \
    OPMAP(FETCH,FETCH) OPMAP(SHIFT,SHIFT) \
    OPMAP(AND,AND) OPMAP(EQV,EQV) OPMAP(OR,OR) \
    OPMAP(NOT,NOT) OPMAP(XOR,XOR) \
    OPMAP(EQL,CMP_EQL) OPMAP(NEQ,CMP_NEQ) \
    OPMAP(LSS,CMP_LSS) OPMAP(LEQ,CMP_LEQ) \
    OPMAP(GTR,CMP_GTR) OPMAP(GEQ,CMP_GEQ) \
    OPMAP(EQLU,CMP_EQLU) OPMAP(NEQU,CMP_NEQU) \
    OPMAP(LSSU,CMP_LSSU) OPMAP(LEQU,CMP_LEQU) \
    OPMAP(GTRU,CMP_GTRU) OPMAP(GEQU,CMP_GEQU) \
    OPMAP(EQLA,CMP_EQLA) OPMAP(NEQA,CMP_NEQA) \
    OPMAP(LSSA,CMP_LSSA) OPMAP(LEQA,CMP_LEQA) \
    OPMAP(GTRA,CMP_GTRA) OPMAP(GEQA,CMP_GEQA)
#define OPMAP(l_,o_) [LEXTYPE_OP_##l_-LEXTYPE_OP_MIN] = OPER_##o_,
static const optype_t opmap[] = {
    OPMAPS
};
#undef OPMAP

static const struct {
    int priority;
    int isr2l;  // 1 if r2l, 3 if unary & r2l
} operinfo[OPER_COUNT] = {
    { 50, 3 }, // fetch
    { 45, 3 }, { 45, 3 }, // unary +/-
    { 40, 0 }, // shift
    { 35, 0 }, { 35, 0 }, { 35, 0 }, // mod * /
    { 30, 0 }, { 30, 0 }, // + -
    { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, // cmp
    { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, // cmp-u
    { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, { 25, 0 }, // cmp-a
    { 20, 3 }, // unary NOT
    { 15, 0 }, { 10, 0 }, { 5, 0 }, { 5, 0 }, // AND, OR, EQV, XOR
    { 0, 1 } // assignment
};

#define DOOPTYPE(t_) "OPER_" #t_,
static const char *oper_names[] = { DOOPTYPES };
#undef DOOPTYPE

static void *fake_label_ptr = (void *)0xffeeeeff;

static int parse_expr(expr_ctx_t ctx, expr_node_t **expp);
static expr_node_t *parse_condexp(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_case(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_select(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_incrdecr(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_wu_loop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_do_loop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_leave(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_exitloop(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static expr_node_t *parse_codecomment(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static int get_ctce(expr_ctx_t ctx, long *valp);

const char *
exprtype_name (exprtype_t type) {
    if (type >= EXPTYPE_COUNT) {
        return "**invalid exptype**";
    }
    return exptypenames[type];
} /* exprtype_name */

const char *
oper_name (optype_t op) {
    if (op >= OPER_COUNT) {
        return "**invalid operator**";
    }
    return oper_names[op];
} /* oper_name */

static int op_is_unary (optype_t op) {
    return (operinfo[op].isr2l & 2) != 0;
}
static int op_is_r2l (optype_t op) {
    return (operinfo[op].isr2l & 1) != 0;
}
static int op_priority (optype_t op) {
    return operinfo[op].priority;
}

static int
check_unary_op (lextype_t lt, optype_t *opp)
{
    if (lt == LEXTYPE_OP_ADD || lt == LEXTYPE_OP_SUB ||
        lt == LEXTYPE_OP_FETCH) {
        *opp = (lt == LEXTYPE_OP_ADD ? OPER_UNARY_PLUS
                : (lt == LEXTYPE_OP_SUB ? OPER_UNARY_MINUS : OPER_FETCH));
        return 1;
    }
    return 0;
}

expr_node_t *
expr_node_alloc (expr_ctx_t ctx, exprtype_t type, textpos_t pos)
{
    expr_node_t *node;
    int i;

    if (ctx->freenodes == 0) {
        ctx->freenodes = malloc(ALLOC_QTY * sizeof(expr_node_t));
        if (ctx->freenodes == 0) {
            return 0;
        }
        memset(ctx->freenodes, 0x66, ALLOC_QTY*sizeof(expr_node_t));
        for (i = 0, node = ctx->freenodes; i < ALLOC_QTY-1; i++, node++) {
            expr_next_set(node, node+1);
        }
        expr_next_set(node, 0);
    }
    node = ctx->freenodes;
    ctx->freenodes = expr_next(node);
    memset(node, 0, sizeof(expr_node_t));
    expr_type_set(node, type);
    expr_textpos_set(node, pos);
    return node;

} /* expr_node_alloc */

void
expr_node_free (expr_ctx_t ctx, expr_node_t *node)
{
    if (node == 0) {
        return;
    }
    switch (expr_type(node)) {
        case EXPTYPE_OPERATOR:
            expr_node_free(ctx, expr_op_lhs(node));
            expr_node_free(ctx, expr_op_rhs(node));
            break;
        case EXPTYPE_PRIM_BLK:
            exprseq_free(ctx, expr_blk_seq(node));
            scope_end(expr_blk_scope(node));
            break;
        case EXPTYPE_PRIM_STRUREF:
            expr_node_free(ctx, expr_struref_accexpr(node));
            break;
        case EXPTYPE_PRIM_FLDREF:
            expr_node_free(ctx, expr_fldref_addr(node));
            expr_node_free(ctx, expr_fldref_pos(node));
            expr_node_free(ctx, expr_fldref_size(node));
            break;
        case EXPTYPE_CTRL_COND:
            expr_node_free(ctx, expr_cond_test(node));
            expr_node_free(ctx, expr_cond_consequent(node));
            expr_node_free(ctx, expr_cond_alternative(node));
            break;
        case EXPTYPE_CTRL_LOOPWU:
            expr_node_free(ctx, expr_wuloop_test(node));
            expr_node_free(ctx, expr_wuloop_body(node));
            break;
        case EXPTYPE_CTRL_LOOPID:
            expr_node_free(ctx, expr_idloop_init(node));
            expr_node_free(ctx, expr_idloop_term(node));
            expr_node_free(ctx, expr_idloop_step(node));
            expr_node_free(ctx, expr_idloop_body(node));
            break;
        case EXPTYPE_PRIM_RTNCALL:
            expr_node_free(ctx, expr_rtnaddr(node));
            exprseq_free(ctx, expr_rtn_inargs(node));
            exprseq_free(ctx, expr_rtn_outargs(node));
            break;

        case EXPTYPE_CTRL_CASE: {
            expr_node_t **actarray = expr_case_cases(node);
            long which, lo, hi;
            expr_node_free(ctx, expr_case_index(node));
            expr_node_free(ctx, expr_case_outrange(node));
            lo = expr_case_lowbound(node);
            hi = expr_case_highbound(node);
            for (which = lo; which <= hi; which++) {
                expr_node_free(ctx, actarray[which-lo]);
            }
            free(actarray);
            break;
        }
        case EXPTYPE_SELECTOR: {
            expr_node_t *sel, *subnext;
            sel = node;
            do {
                subnext = expr_selector_next(sel);
                expr_node_free(ctx, expr_selector_action(sel));
                expr_node_free(ctx, expr_selector_low(sel));
                expr_node_free(ctx, expr_selector_high(sel));
                expr_node_free(ctx, sel);
                sel = subnext;
            } while (sel != 0);

            break;
        }
        case EXPTYPE_CTRL_SELECT:
            expr_node_free(ctx, expr_sel_index(node));
            exprseq_free(ctx, expr_sel_selectors(node));
            break;

        case EXPTYPE_CTRL_EXIT:
            expr_node_free(ctx, expr_exit_value(node));
            break;

        case EXPTYPE_PRIM_LIT:
            string_free(expr_litstring(node));
            break;

        case EXPTYPE_NOOP:
        case EXPTYPE_PRIM_SEG:
        case EXPTYPE_EXECFUN: // TBD XXX
            break;
    }
    memset(node, 0x77, sizeof(expr_node_t));
    expr_next_set(node, ctx->freenodes);
    ctx->freenodes = node;

} /* expr_node_free */

expr_node_t *
expr_node_copy (expr_ctx_t ctx, expr_node_t *node)
{
    expr_node_t *dst;

    if (node == 0) {
        return 0;
    }

    dst = expr_node_alloc(ctx, expr_type(node), expr_textpos(node));
    memcpy(dst, node, sizeof(expr_node_t));

    switch (expr_type(node)) {
        case EXPTYPE_OPERATOR:
            expr_op_lhs_set(dst, expr_node_copy(ctx, expr_op_lhs(node)));
            expr_op_rhs_set(dst, expr_node_copy(ctx, expr_op_rhs(node)));
            break;
        case EXPTYPE_PRIM_BLK:
            exprseq_init(expr_blk_seq(dst));
            exprseq_copy(ctx, expr_blk_seq(dst), expr_blk_seq(node));
            expr_blk_scope_set(dst,
                               scope_copy(expr_blk_scope(node),
                                          scope_getparent(expr_blk_scope(node))));
            break;
        case EXPTYPE_PRIM_STRUREF:
            expr_struref_accexpr_set(dst, expr_node_copy(ctx, expr_struref_accexpr(node)));
            break;
        case EXPTYPE_PRIM_FLDREF:
            expr_fldref_addr_set(dst, expr_node_copy(ctx, expr_fldref_addr(node)));
            expr_fldref_pos_set(dst, expr_node_copy(ctx, expr_fldref_pos(node)));
            expr_fldref_size_set(dst, expr_node_copy(ctx, expr_fldref_size(node)));
            break;
        case EXPTYPE_CTRL_COND:
            expr_cond_test_set(dst, expr_node_copy(ctx, expr_cond_test(node)));
            expr_cond_consequent_set(dst,
                                     expr_node_copy(ctx, expr_cond_consequent(node)));
            expr_cond_alternative_set(dst,
                                      expr_node_copy(ctx, expr_cond_alternative(node)));
            break;
        case EXPTYPE_CTRL_LOOPWU:
            expr_wuloop_test_set(dst, expr_node_copy(ctx, expr_wuloop_test(node)));
            expr_wuloop_body_set(dst, expr_node_copy(ctx, expr_wuloop_body(node)));
            break;
        case EXPTYPE_CTRL_LOOPID:
            expr_idloop_init_set(dst, expr_node_copy(ctx, expr_idloop_init(node)));
            expr_idloop_term_set(dst, expr_node_copy(ctx, expr_idloop_term(node)));
            expr_idloop_step_set(dst, expr_node_copy(ctx, expr_idloop_step(node)));
            expr_idloop_body_set(dst, expr_node_copy(ctx, expr_idloop_body(node)));
            break;
        case EXPTYPE_PRIM_RTNCALL:
            expr_rtnaddr_set(dst, expr_node_copy(ctx, expr_rtnaddr(node)));
            exprseq_init(expr_rtn_inargs(dst));
            exprseq_init(expr_rtn_outargs(dst));
            exprseq_copy(ctx, expr_rtn_inargs(dst), expr_rtn_inargs(node));
            exprseq_copy(ctx, expr_rtn_outargs(dst), expr_rtn_outargs(node));
            break;

        case EXPTYPE_CTRL_CASE: {
            expr_node_t **actarray = expr_case_cases(node);
            expr_node_t **dstarray;
            long which, lo, hi;
            expr_case_index_set(dst, expr_node_copy(ctx, expr_case_index(node)));
            expr_case_outrange_set(dst, expr_node_copy(ctx, expr_case_outrange(node)));
            lo = expr_case_lowbound(node);
            hi = expr_case_highbound(node);
            dstarray = malloc((hi-lo+1)*sizeof(expr_node_t *));
            memset(dstarray, 0, ((hi-lo+1)*sizeof(expr_node_t *)));
            for (which = lo; which <= hi; which++) {
                dstarray[which-lo] = expr_node_copy(ctx, actarray[which-lo]);
            }
            expr_case_actions_set(dst, dstarray);
            break;
        }
        case EXPTYPE_SELECTOR: {
            expr_node_t *sub, *dslast, *dsub;
            expr_selector_action_set(dst,
                                     expr_node_copy(ctx, expr_selector_action(node)));
            expr_selector_lohi_set(dst,
                                   expr_node_copy(ctx, expr_selector_low(node)),
                                   expr_node_copy(ctx, expr_selector_high(node)));
            dslast = dst;
            for (sub = expr_selector_next(node); sub != 0;
                 sub = expr_selector_next(sub)) {
                dsub = expr_node_alloc(ctx, EXPTYPE_SELECTOR, expr_textpos(sub));
                expr_selector_action_set(dsub,
                                         expr_node_copy(ctx, expr_selector_action(sub)));
                expr_selector_lohi_set(dsub,
                                       expr_node_copy(ctx, expr_selector_low(sub)),
                                       expr_node_copy(ctx, expr_selector_high(sub)));
                expr_selector_next_set(dslast, dsub);
                dslast = dsub;
            }
            break;
        }
        case EXPTYPE_CTRL_SELECT:
            expr_sel_index_set(dst, expr_node_copy(ctx, expr_sel_index(node)));
            exprseq_init(expr_sel_selectors(dst));
            exprseq_copy(ctx, expr_sel_selectors(dst), expr_sel_selectors(node));
            break;

        case EXPTYPE_CTRL_EXIT:
            expr_exit_value_set(dst, expr_node_copy(ctx, expr_exit_value(node)));
            break;
            
        case EXPTYPE_PRIM_LIT:
            if (expr_litstring(node) != 0) {
                expr_litstring_set(dst, string_copy(0, expr_litstring(node)));
            }
            break;
        case EXPTYPE_NOOP:
        case EXPTYPE_EXECFUN: // TBD XXX
        case EXPTYPE_PRIM_SEG:
            break;
    }

    return dst;

} /* expr_node_copy */

void
exprseq_free (expr_ctx_t ctx, exprseq_t *seq) {
    expr_node_t *exp;

    if (seq == 0) {
        return;
    }
    for (exp = exprseq_remhead(seq); exp != 0; exp = exprseq_remhead(seq)) {
        expr_node_free(ctx, exp);
    }

} /* exprseq_free */

void
exprseq_copy (expr_ctx_t ctx, exprseq_t *dst, exprseq_t *src)
{
    expr_node_t *exp;

    if (dst == 0 || src == 0) {
        return;
    }

    for (exp = exprseq_head(src); exp != 0; exp = expr_next(exp)) {
        exprseq_instail(dst, expr_node_copy(ctx, exp));
    }

} /* exprseq_copy */

static optype_t
lextype_to_optype (lextype_t lt)
{
    if (lt < LEXTYPE_OP_MIN || lt > LEXTYPE_OP_MAX) {
        return OPER_NONE;
    }
    return opmap[lt-LEXTYPE_OP_MIN];
}

static expr_node_t *
parse_plit (expr_ctx_t ctx, lextype_t curlt, lexeme_t *lex)
{
    parse_ctx_t pctx = ctx->pctx;
    name_t *plitname;
    expr_node_t *exp;

    plitname = define_plit(ctx, curlt, lexeme_textpos_get(lex));
    if (plitname == 0) {
        /* XXX error condition */
        return 0;
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, parser_curpos(pctx));
    expr_seg_name_set(exp, plitname);
    expr_is_ltce_set(exp, 1);
    return exp;

} /* parse_plit */

void
expr_dispatch_register (expr_ctx_t ctx, lextype_t lt, expr_dispatch_fn fn)
{
    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        ctx->name_dispatchers[lt-LEXTYPE_NAME_MIN] = fn;
        return;
    }
    if (lt < LEXTYPE_EXPKWD_MIN || lt > LEXTYPE_EXPKWD_MAX) {
        /* XXX error condition */
        return;
    }
    ctx->dispatchers[lt-LEXTYPE_EXPKWD_MIN] = fn;
}

expr_dispatch_fn
lookup_dispatcher (expr_ctx_t ctx, lextype_t lt)
{
    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        return ctx->name_dispatchers[lt-LEXTYPE_NAME_MIN];
    }
    if (lt >= LEXTYPE_EXPKWD_MIN && lt <= LEXTYPE_EXPKWD_MAX) {
        return ctx->dispatchers[lt-LEXTYPE_EXPKWD_MIN];
    }
    return 0;
}

expr_ctx_t
expr_init (parse_ctx_t pctx, stgctx_t stg, scopectx_t kwdscope)
{
    expr_ctx_t ectx;
    int i;

    ectx = malloc(sizeof(struct expr_ctx_s));
    if (ectx == 0) {
        return 0;
    }
    memset(ectx, 0, sizeof(struct expr_ctx_s));
    ectx->pctx = pctx;
    parser_set_expctx(pctx, ectx);
    ectx->stg = stg;
    ectx->mach = parser_get_machinedef(pctx);
    ectx->namectx = scope_namectx(kwdscope);
    exprseq_init(&ectx->exprseq);

    for (i = 0; i < sizeof(expr_names)/sizeof(expr_names[0]); i++) {
        name_declare(kwdscope, &expr_names[i], 0, 0, 0, 0);
    }

    expr_dispatch_register(ectx, LEXTYPE_KWD_PLIT, parse_plit);
    expr_dispatch_register(ectx, LEXTYPE_KWD_UPLIT, parse_plit);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_IF, parse_condexp);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_CASE, parse_case);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECT, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECTA, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECTU, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECTONE, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECTONEA, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_SELECTONEU, parse_select);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_INCR, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_INCRA, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_INCRU, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_DECR, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_DECRA, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_DECRU, parse_incrdecr);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_WHILE, parse_wu_loop);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_UNTIL, parse_wu_loop);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_DO, parse_do_loop);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_LEAVE, parse_leave);
    expr_dispatch_register(ectx, LEXTYPE_CTRL_EXITLOOP, parse_exitloop);
    expr_dispatch_register(ectx, LEXTYPE_KWD_CODECOMMENT, parse_codecomment);

    declarations_init(ectx, pctx, kwdscope, stg, ectx->mach);

    return ectx;
    
} /* expr_init */

parse_ctx_t expr_parse_ctx (expr_ctx_t ctx) { return ctx->pctx; }
namectx_t expr_namectx (expr_ctx_t ctx) { return ctx->namectx; }
stgctx_t expr_stg_ctx (expr_ctx_t ctx) { return ctx->stg; }
machinedef_t *expr_machinedef (expr_ctx_t ctx) { return ctx->mach; }

/*
 * parse_block
 *
 * Can be preceded by CODECOMMENT 'qstring':
 * Can be preceded by label:...
 * BEGIN or (
 *   {declarations or nothing}
 *   {actions or nothing}
 *   {value or nothing}
 * END or )
 */
static int
parse_block (expr_ctx_t ctx, lextype_t curlt, expr_node_t **expp,
             strdesc_t *codecomment, namereflist_t *labels) {

    parse_ctx_t pctx = ctx->pctx;
    lextype_t lt;
    lexeme_t *lex;
    nameref_t *ref;
    scopectx_t scope = 0;
    expr_node_t *exp = 0;
    exprseq_t seq;
    expr_node_t *valexp;
    namectx_t namectx = scope_namectx(parser_scope_get(pctx));
    textpos_t endpos;
    lextype_t closer = (curlt == LEXTYPE_EXP_DELIM_BEGIN ?
                        LEXTYPE_EXP_DELIM_END : LEXTYPE_DELIM_RPAR);


    // Set a special value for the labels so that
    // LEAVE processing will know that the labels are OK
    for (ref = namereflist_head(labels); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0 && name_value_pointer(ref->np) == 0) {
            name_value_pointer_set(ref->np, fake_label_ptr);
        }
    }
    lt = parser_next(pctx, QL_NORMAL, &lex);

    exprseq_init(&seq);
    valexp = 0;
    while (1) {
        if (lt == closer) {
            endpos = lexeme_textpos_get(lex);
            lexeme_free(lex);
            break;
        } else if (lt >= LEXTYPE_DCL_MIN && lt <= LEXTYPE_DCL_MAX) {
            if (exprseq_length(&seq) != 0) {
                /* XXX error condition */
                lexeme_free(lex);
                parser_skip_to_delim(pctx, closer);
                endpos = parser_curpos(pctx);
                break;
            }
            if (scope == 0) {
                scope = parser_scope_begin(pctx);
            }
            parser_insert(pctx, lex);
            parse_declaration(ctx);
            lt = parser_next(pctx, QL_NORMAL, &lex);
            continue;
        } else {
            parser_insert(pctx, lex);
        }
        exp = 0;
        if (!parse_expr(ctx, &exp)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, closer);
            endpos = parser_curpos(pctx);
            break;
        }
        valexp = exp;
        exprseq_instail(&seq, exp);
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_SEMI) {
            valexp = 0;
            lexeme_free(lex);
            lt = parser_next(pctx, QL_NORMAL, &lex);
        }
    }

    if (scope == 0 && codecomment == 0 && namereflist_empty(labels)) {
        if (exprseq_length(&seq) == 0) {
            *expp = expr_node_alloc(ctx, EXPTYPE_NOOP, endpos);
            return 1;
        }
        if (exprseq_length(&seq) == 1) {
            *expp = exprseq_remhead(&seq);
            return 1;
        }
    }
    if (scope != 0) {
        parser_scope_end(pctx);
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_BLK, endpos);
    expr_blk_scope_set(exp, scope);
    
    exprseq_set(expr_blk_seq(exp), &seq);
    expr_blk_valexp_set(exp, valexp);
    expr_blk_codecomment_set(exp, codecomment);
    *expp = exp;
    // Now point the labels at us
    while ((ref = namereflist_remhead(labels))) {
        if (ref->np == 0 || name_value_pointer(ref->np) != fake_label_ptr) {
            /* XXX error condition */
        }
        name_value_pointer_set(ref->np, exp);
        nameref_free(namectx, ref);
    }

    return 1;

} /* parse_block */

static expr_node_t *
parse_leave (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *exp, *valexp;
    lexeme_t *lex;
    name_t *lp;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
        /* XXX error condition */
    }
    lp = lexeme_ctx_get(lex);
    if (lp == 0 || name_value_pointer(lp) != fake_label_ptr) {
        /* XXX error condition */
    }
    valexp = 0;
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_WITH, 0, 1)) {
        if (!parse_expr(ctx, &valexp)) {
            /* XXX error condition */
        }
    }
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_EXIT, lexeme_textpos_get(lex));
    expr_exit_label_set(exp, lp);
    expr_exit_value_set(exp, valexp);
    expr_has_value_set(exp, (valexp != 0));
    return exp;

} /* parse_leave */

static expr_node_t *
parse_exitloop (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    expr_node_t *exp, *valexp;

    if (ctx->loopdepth == 0) {
        /* XXX error condition */
    }
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_EXIT, parser_curpos(ctx->pctx));
    valexp = 0;
    if (parse_expr(ctx, &valexp)) {
        expr_exit_value_set(exp, valexp);
        expr_has_value_set(exp, 1);
    }
    return exp;

} /* parse_exitloop */

static expr_node_t *
parse_wu_loop (expr_ctx_t ctx, lextype_t opener, lexeme_t *curlex)
{
    expr_node_t *body, *test, *exp;

    body = test = 0;
    if (!parse_expr(ctx, &test)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(ctx->pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    ctx->loopdepth += 1;
    if (!parse_expr(ctx, &body)) {
        ctx->loopdepth -= 1;
        /* XXX error condition */
        return 0;
    }
    ctx->loopdepth -= 1;

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPWU, parser_curpos(ctx->pctx));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, LOOP_PRETEST);
    expr_wuloop_body_set(exp, body);
    // XXX should evaluate the loop exits for values
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_wu_loop */

static expr_node_t *
parse_do_loop (expr_ctx_t ctx, lextype_t opener, lexeme_t *curlex)
{
    lextype_t lt;
    lexeme_t *lex;
    expr_node_t *body, *test, *exp;

    body = test = 0;
    ctx->loopdepth += 1;
    if (!parse_expr(ctx, &body)) {
        ctx->loopdepth -= 1;
        /* XXX error condition */
        return 0;
    }
    ctx->loopdepth -= 1;

    lt = parser_next(ctx->pctx, QL_NORMAL, &lex);
    if (lt != LEXTYPE_CTRL_WHILE && lt != LEXTYPE_CTRL_UNTIL) {
        /* XXX error condition */
        parser_insert(ctx->pctx, lex);
        return 0;
    }
    lexeme_free(lex);

    if (!parse_expr(ctx, &test)) {
        /* XXX error condition */
        return 0;
    }

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPWU, parser_curpos(ctx->pctx));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, LOOP_POSTTEST);
    expr_wuloop_body_set(exp, body);
    // XXX should evaluate the loop exits for values
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_do_loop */

/*
 * parse_arglist
 *
 * NB: assumes that we've just seen the opener - left paren
 * for ordinary calls, routine-address (but NOT the comma)
 * for general calls
 *
 * Assumes the routine-designator expression is at the top
 * of the expr tree
 */
expr_node_t *
expr_parse_arglist (expr_ctx_t ctx, expr_node_t *rtn)
{
    parse_ctx_t pctx = ctx->pctx;
    exprseq_t inargs, outargs;
    int doing_outargs;
    expr_node_t *arg, *exp;

    exprseq_init(&inargs);
    exprseq_init(&outargs);
    doing_outargs = parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        while (1) {
            if (!parse_expr(ctx, &arg)) {
                arg = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
                // null argument = zero literal
            }

            exprseq_instail((doing_outargs ? &outargs : &inargs), arg);

            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                break;
            }

            if (!doing_outargs &&
                parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
                doing_outargs = 1;
                if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                    break;
                }
                continue;
            }

            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                /* XXX error condition */
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
                return 0;
            }
            
        } /* argument loop */
    }

    // validate the arguments against the routine declaration, if we can
    // Insert the routine-call into the expression tree
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_RTNCALL, parser_curpos(pctx));
    expr_rtnaddr_set(exp, rtn);
    exprseq_set(expr_rtn_inargs(exp), &inargs);
    exprseq_set(expr_rtn_outargs(exp), &outargs);
    return exp;

} /* parse_arglist */

static expr_node_t *
lex_to_expr (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    expr_node_t *exp = 0;

    switch (lt) {
        case LEXTYPE_NUMERIC:
            exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, lexeme_textpos_get(lex));
            expr_litval_set(exp, lexeme_signedval(lex));
            expr_is_ctce_set(exp, 1);
            break;
        case LEXTYPE_STRING: {
            int i;
            strdesc_t *text = lexeme_text(lex);
            unsigned long val = 0;
            int len = text->len;
            if (len > machine_scalar_maxbytes(ctx->mach) && !ctx->longstringsok) {
                /* XXX error condition */
                len = machine_scalar_maxbytes(ctx->mach);
            }
            exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, lexeme_textpos_get(lex));
            expr_litstring_set(exp, string_copy(0, text));
            for (i = 0; i < len; i++) {
                val = val | (*(text->ptr+i) << (i*8));
            }
            expr_litval_set(exp, val);
            expr_is_ctce_set(exp, 1);
            break;
        }
        case LEXTYPE_SEGMENT:
            exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, lexeme_textpos_get(lex));
            expr_seg_base_set(exp, lexeme_ctx_get(lex));
            expr_seg_units_set(exp, machine_scalar_units(ctx->mach));
            expr_seg_signext_set(exp, 0);
            expr_is_ltce_set(exp, seg_addr_is_ltce(expr_seg_base(exp)));
            break;
        default:
            /* XXX huh? */
            break;
    }

    if (exp != 0) {
        expr_has_value_set(exp, 1);
    }
    return exp;

} /* lex_to_expr */

static int
parse_fldref (expr_ctx_t ctx, expr_node_t **expp) {

    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *pos, *size, *exp;
    long signext = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LANGLE, 0, 1)) {
        return 0;
    }
    pos = size = 0;
    if (!parse_expr(ctx, &pos)) {
        /* XXX error condition */
        pos = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        /* XXX error condition */
    }
    if (!parse_expr(ctx, &size)) {
        /* XXX error condition */
        size = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        if (!get_ctce(ctx, &signext) || (signext != 0 && signext != 1)) {
            /* XXX error condition */
            signext = 0;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RANGLE, 0, 1)) {
        /* XXX error condition */
    }

    if (expr_type(pos) == EXPTYPE_PRIM_LIT && expr_type(size) == EXPTYPE_PRIM_LIT) {
        machinedef_t *mach = parser_get_machinedef(pctx);

        // Can operate on CTCEs directly
        if (expr_type(*expp) == EXPTYPE_PRIM_LIT) {
            expr_litval_set(*expp, getvalue(expr_litval(*expp) >> expr_litval(pos),
                                            (unsigned int) expr_litval(size),
                                            (signext != 0)));
            expr_node_free(ctx, pos);
            expr_node_free(ctx, size);
            return 1;
        }
        if (expr_litval(pos) % machine_unit_bits(mach) == 0 &&
            expr_litval(size) == machine_scalar_bits(mach)) {
            expr_seg_offset_set(*expp, expr_seg_offset(*expp) +
                                expr_litval(pos) / machine_unit_bits(mach));
            expr_seg_units_set(*expp, machine_scalar_units(mach));
            expr_seg_signext_set(*expp, (signext != 0));
            expr_node_free(ctx, pos);
            expr_node_free(ctx, size);
            return 1;
        }
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_FLDREF, parser_curpos(pctx));
    expr_fldref_addr_set(exp, *expp);
    expr_is_ctce_set(exp, expr_is_ctce(*expp));
    expr_is_ltce_set(exp, expr_is_ltce(*expp));
    expr_fldref_pos_set(exp, pos);
    expr_fldref_size_set(exp, size);
    expr_fldref_signext_set(exp, (signext != 0));
    expr_has_value_set(exp, 1);
    *expp = exp;
    return 1;

} /* parse_fldref */

static expr_node_t *
parse_codecomment (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    expr_node_t *exp = 0;
    parse_ctx_t pctx = ctx->pctx;
    namectx_t namectx = scope_namectx(parser_scope_get(pctx));
    strdesc_t *codecomment = 0;
    namereflist_t labels;
    lexeme_t *lex;

    namereflist_init(&labels);
    while (parser_expect(pctx, QL_NORMAL, LEXTYPE_STRING, &lex, 1)) {
        codecomment = string_append(codecomment, lexeme_text(lex));
        lexeme_free(lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
        /* XXX error condition, but assume it was missed */
    }
    while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
        namereflist_instail(&labels,
                       nameref_alloc(namectx, lexeme_ctx_get(lex)));
        lexeme_free(lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition */
        }
    }
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt != LEXTYPE_DELIM_LPAR && lt != LEXTYPE_EXP_DELIM_BEGIN) {
        /* XXX error condition */
        string_free(codecomment);
        return 0;
    }
    lexeme_free(lex);
    if (!parse_block(ctx, lt, &exp, codecomment, &labels)) {
        return 0;
    }
    return exp;
}

static expr_node_t *
parse_primary (expr_ctx_t ctx, lextype_t lt, lexeme_t *lex)
{
    expr_node_t *exp = 0;
    parse_ctx_t pctx = ctx->pctx;
    namectx_t namectx = scope_namectx(parser_scope_get(pctx));
    namereflist_t labels;

    namereflist_init(&labels);
    if (lt == LEXTYPE_NAME_LABEL) {
        namereflist_instail(&labels, nameref_alloc(namectx, lexeme_ctx_get(lex)));
        lexeme_free(lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition */
        }
        while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
            namereflist_instail(&labels,
                                nameref_alloc(namectx, lexeme_ctx_get(lex)));
            lexeme_free(lex);
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                /* XXX error condition */
            }
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
            if (!parse_block(ctx, lt, &exp, 0, &labels)) {
                return 0;
            }
        } else {
            /* XXX error condition */
            return 0;
        }
    } else if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
        lexeme_free(lex);
        if (!parse_block(ctx, lt, &exp, 0, &labels)) {
            return 0;
        }
    } else {
        exp = lex_to_expr(ctx, lt, lex);
        if (exp != 0) {
            lexeme_free(lex);
        }
    }

    if (exp == 0) {
        return exp;
    }

// handle this through the dispatch functions
//    if (lt == LEXTYPE_NAME_FUNCTION) { // XXX Fix this
//        exp = expr_node_alloc(ctx, EXPTYPE_EXECFUN, lexeme_textpos_get(lex));
//        // XXX need to parse the entire function invocation here
//        if (parse_fldref(ctx, &exp)) {
//            // and exp is updated to be a PRIM_FLDREF
//            lexeme_free(lex);
//            return 1;
//        } else {
//            expr_node_free(ctx, exp);
//            return 0; // executable-functions are not, by themselves, primaries
//        }

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_node_t *rtncall = expr_parse_arglist(ctx, exp);
        if (rtncall == 0) {
            /* XXX error condition */
            expr_node_free(ctx, exp);
            return 0;
        }
        exp = rtncall;
    }

    // XXX Must also check for general routine calls

    // At this point, 'exp' is the primary expression.
    // If it's not already a field-ref, see if we've got one.
    if (expr_type(exp) != EXPTYPE_PRIM_FLDREF) {
        parse_fldref(ctx, &exp);
    }

    return exp;

} /* parse_primary */

static seg_t *
find_base_seg (expr_node_t *expr)
{
    seg_t *seg;
    if (expr == 0) {
        return 0;
    }
    if (expr_type(expr) == EXPTYPE_PRIM_SEG) {
        return expr_seg_base(expr);
    }
    if (expr_type(expr) != EXPTYPE_OPERATOR) {
        return 0;
    }
    seg = find_base_seg(expr_op_lhs(expr));
    if (seg == 0) {
        seg = find_base_seg(expr_op_rhs(expr));
    }
    return seg;
}

static void
update_xtce_bits (expr_node_t *opexpr)
{
    optype_t op = expr_op_type(opexpr);

    if (op == OPER_FETCH || op == OPER_ASSIGN) {
        expr_is_ctce_set(opexpr, 0);
        expr_is_ltce_set(opexpr, 0);
        return;
    }
    if (op_is_unary(op)) {
        expr_is_ctce_set(opexpr, expr_is_ctce(expr_op_rhs(opexpr)));
        expr_is_ltce_set(opexpr, 0);
        return;
    }
    if (expr_is_ctce(expr_op_lhs(opexpr)) && expr_is_ctce(expr_op_rhs(opexpr))) {
        expr_is_ctce_set(opexpr, 1);
        return;
    }
    if (expr_is_ltce_only(expr_op_lhs(opexpr))) {
        if ((op == OPER_ADD || op == OPER_SUBTRACT) &&
            expr_is_ctce(expr_op_rhs(opexpr))) {
            expr_is_ltce_set(opexpr, 1);
            return;
        }
        if ((op == OPER_SUBTRACT ||
             (op >= OPER_CMP_EQLA && op <= OPER_CMP_GEQA)) &&
             expr_is_ltce_only(expr_op_rhs(opexpr))) {
            seg_t *seg_lhs, *seg_rhs;
            seg_lhs = find_base_seg(expr_op_lhs(opexpr));
            seg_rhs = find_base_seg(expr_op_rhs(opexpr));
            if (seg_lhs != 0 && seg_rhs != 0) {
                expr_is_ltce_set(opexpr,
                                 seg_static_psect(seg_lhs) == seg_static_psect(seg_rhs));
            }
            return;
        }
    }
    if (expr_is_ltce_only(expr_op_rhs(opexpr))) {
        if (op == OPER_ADD &&
            expr_is_ctce(expr_op_lhs(opexpr))) {
            expr_is_ltce_set(opexpr, 1);
            return;
        }
    }
}

static expr_node_t *
parse_op_expr (expr_ctx_t ctx, optype_t curop, expr_node_t *lhs)
{
    expr_node_t *thisnode, *rhs;
    optype_t op;
    int normal;

    if (expr_is_noop(lhs)) {
        if (!op_is_unary(curop)) {
            /* XXX error condition */
            expr_node_free(ctx, lhs);
            lhs = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(ctx->pctx));
            expr_has_value_set(lhs, 1);
            // but try anyway?
        }
    } else {
        if (expr_is_ctrl(lhs)) {
            /* XXX error condition */
            // but try anyway
            expr_node_free(ctx, lhs);
            lhs = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(ctx->pctx));
            expr_has_value_set(lhs, 1);
        }
    }

    rhs = 0;
    ctx->noreduce += 1;
    if (!parse_expr(ctx, &rhs) || rhs == 0) {
        // XXX error condition ?
        ctx->noreduce -= 1;
        return 0;
    }
    ctx->noreduce -= 1;

    if (expr_is_noop(rhs) || expr_is_ctrl(rhs)) {
        /* XXX error condition */
    }

    /*
     * When we have another operator expression on our
     * right-hand side, check the operator priority.
     * If RHS's priority is greater than this node's, OR if
     *    it's the same but has r-to-l associativity,
     *  then we swap positions - putting RHS on top
     *  and putting ourselves in as its LHS.
     */
    normal = 1;
    if (expr_is_opexp(rhs)) {
        normal = 0;
        op = expr_op_type(rhs);
        if (op_is_unary(op)) {
            if (op_priority(op) < op_priority(curop)) {
                /* XXX error condition, but continue ? */
            }
            normal = 1;
        }
        if (op_priority(op) > op_priority(curop) ||
            (op_priority(op) == op_priority(curop) &&
             op_is_r2l(op))) {
                normal = 1;
            }
    }

    thisnode = expr_node_alloc(ctx, EXPTYPE_OPERATOR, expr_textpos(rhs));
    if (thisnode == 0) {
        /* XXX error condition */
        return 0;
    }
    if (normal) {
        expr_op_type_set(thisnode, curop);
        expr_op_lhs_set(thisnode, lhs);
        expr_op_rhs_set(thisnode, rhs);
        expr_has_value_set(thisnode, 1);
        update_xtce_bits(thisnode);
        return thisnode;
    } else {
        expr_op_type_set(thisnode, curop);
        expr_op_rhs_set(thisnode, expr_op_lhs(rhs));
        expr_op_lhs_set(rhs, thisnode);
        expr_has_value_set(thisnode, 1);
        update_xtce_bits(thisnode);
        update_xtce_bits(rhs);
        return rhs;
    }

} /* parse_op_expr */

void
reduce_op_expr (expr_ctx_t ctx, expr_node_t **nodep) {
    expr_node_t *node, *lhs, *rhs;

    if (ctx->noreduce || nodep == 0) {
        return;
    }

    node = *nodep;

    if (!expr_is_opexp(node)) {
        return;
    }

    lhs = expr_op_lhs(node);
    if (expr_is_opexp(lhs)) {
        reduce_op_expr(ctx, &lhs);
    }
    rhs = expr_op_rhs(node);
    if (expr_is_opexp(rhs)) {
        reduce_op_expr(ctx, &rhs);
    }
    // Check for unary +/- of a CTCE
    if (expr_is_noop(lhs)) {
        optype_t op = expr_op_type(node);
        if (op == OPER_UNARY_MINUS || op == OPER_UNARY_PLUS) {
            if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
                long result = (op == OPER_UNARY_MINUS ?
                               -expr_litval(rhs) : expr_litval(rhs));
                expr_node_free(ctx, rhs);
                expr_node_free(ctx, lhs);
                expr_type_set(node, EXPTYPE_PRIM_LIT);
                expr_litval_set(node, result);
            }
        }
        return;
    }

    // Check for operations on CTCEs
    if (expr_type(lhs) == EXPTYPE_PRIM_LIT) {
        long leftval = expr_litval(lhs);
        if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
            long rightval = expr_litval(rhs);
            long result;
            switch (expr_op_type(node)) {
                case OPER_ADD:
                    result = leftval + rightval;
                    break;
                case OPER_SUBTRACT:
                    result = leftval - rightval;
                    break;
                case OPER_MULT:
                    result = leftval * rightval;
                    break;
                case OPER_DIV:
                    if (rightval == 0) {
                        /* XXX error condition */
                        result = 0;
                    } else {
                        result = leftval / rightval;
                    }
                    break;
                case OPER_MODULO:
                    if (rightval == 0) {
                        /* XXX error condition */
                        result = 0;
                    } else {
                        result = leftval % rightval;
                    }
                    break;
                case OPER_SHIFT:
                    result = (rightval > 0 ? leftval << rightval
                              : leftval >> (-rightval));
                    break;
                case OPER_AND:
                    result = leftval & rightval;
                    break;
                case OPER_OR:
                    result = leftval | rightval;
                    break;
                case OPER_XOR:
                    result = leftval ^ rightval;
                    break;
                case OPER_EQV:
                    result = ~(leftval ^ rightval);
                    break;
                case OPER_CMP_EQL:
                    result = (leftval == rightval) ? 1 : 0;
                    break;
                case OPER_CMP_NEQ:
                    result = (leftval != rightval) ? 1 : 0;
                    break;
                case OPER_CMP_LSS:
                    result = (leftval < rightval) ? 1 : 0;
                    break;
                case OPER_CMP_LEQ:
                    result = (leftval <= rightval) ? 1 : 0;
                    break;
                case OPER_CMP_GTR:
                    result = (leftval > rightval) ? 1 : 0;
                    break;
                case OPER_CMP_GEQ:
                    result = (leftval >= rightval) ? 1 : 0;
                    break;
                case OPER_CMP_EQLU:
                    result = ((unsigned long)leftval == (unsigned long) rightval ?
                              1 : 0);
                    break;
                case OPER_CMP_NEQU:
                    result = ((unsigned long)leftval != (unsigned long) rightval ?
                              1 : 0);
                    break;
                case OPER_CMP_LSSU:
                    result = ((unsigned long)leftval < (unsigned long) rightval ?
                              1 : 0);
                    break;
                case OPER_CMP_LEQU:
                    result = ((unsigned long)leftval <= (unsigned long) rightval ?
                              1 : 0);
                    break;
                case OPER_CMP_GTRU:
                    result = ((unsigned long)leftval > (unsigned long) rightval ?
                              1 : 0);
                    break;
                case OPER_CMP_GEQU:
                    result = ((unsigned long)leftval >= (unsigned long) rightval ?
                              1 : 0);
                    break;
                default:
                    return;
            }
            expr_node_free(ctx, lhs);
            expr_node_free(ctx, rhs);
            expr_type_set(node, EXPTYPE_PRIM_LIT);
            expr_litval_set(node, result);
            expr_litstring_set(node, 0);
            return;
        }
    }

    // Not a CTCE.  See if it's add-to-zero.  If so,
    // replace node with the other term.
    // Also check for SEG+offset or offset+SEG,
    // and create SEG expression with the offset included.
    if (expr_op_type(node) == OPER_ADD) {
        if (expr_type(lhs) == EXPTYPE_PRIM_LIT) {
            if (expr_litval(lhs) == 0) {
                expr_node_t *tmp = rhs;
                expr_op_rhs_set(node, 0);
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
            if (expr_type(rhs) == EXPTYPE_PRIM_SEG) {
                expr_node_t *tmp = rhs;
                expr_op_rhs_set(node, 0);
                expr_seg_name_set(tmp, expr_seg_name(rhs));
                expr_seg_offset_set(tmp, expr_seg_offset(tmp)+expr_litval(lhs));
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
        }
        if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
            if (expr_litval(rhs) == 0) {
                expr_node_t *tmp = lhs;
                expr_op_lhs_set(node, 0);
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
            if (expr_type(lhs) == EXPTYPE_PRIM_SEG) {
                expr_node_t *tmp = lhs;
                expr_op_lhs_set(node, 0);
                expr_seg_name_set(tmp, expr_seg_name(lhs));
                expr_seg_offset_set(tmp, expr_seg_offset(tmp)+expr_litval(rhs));
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
        }
    }

    // Now check for multiply-by-1, which again
    // reduces to just the other term.
    if (expr_op_type(node) == OPER_MULT) {
        if (expr_type(lhs) == EXPTYPE_PRIM_LIT &&
            expr_litval(lhs) == 1) {
            expr_node_t *tmp = rhs;
            expr_op_rhs_set(node, 0);
            expr_node_free(ctx, node);
            *nodep = tmp;
            return;
        }
        if (expr_type(rhs) == EXPTYPE_PRIM_LIT &&
            expr_litval(rhs) == 1) {
            expr_node_t *tmp = lhs;
            expr_op_lhs_set(node, 0);
            expr_node_free(ctx, node);
            *nodep = tmp;
            return;
        }
    }

    // Now check for subtracting zero,
    // and for segment - constant offset
    if (expr_op_type(node) == OPER_SUBTRACT) {
        if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
            if (expr_litval(rhs) == 0) {
                expr_node_t *tmp = lhs;
                expr_op_lhs_set(node, 0);
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
            if (expr_type(lhs) == EXPTYPE_PRIM_SEG) {
                expr_node_t *tmp = lhs;
                expr_op_lhs_set(node, 0);
                expr_seg_offset_set(tmp, expr_seg_offset(tmp)-expr_litval(rhs));
                expr_node_free(ctx, node);
                *nodep = tmp;
                return;
            }
        }
    }

} /* reduce_op_expr */

/*
 * The workhorse expression-parsing routine.
 *
 * Can be called recursively (by parse_op_expr); if
 * the 'recursed' flag is set, we know we are in the
 * middle of parsing an operator expression, so we
 * don't do any reduction.
 *
 * The 'lstrok' flags is passed to parse_primary,
 * since in certain contexts (notably %ISSTRING()),
 * we don't want to warn about strings longer that
 * can't fit into a fullword value.  In most cases, we do.
 */
static int
parse_expr (expr_ctx_t ctx, expr_node_t **expp)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_dispatch_fn dfunc;
    expr_node_t *exp;
    lexeme_t *lex;
    lextype_t lt;
    optype_t op;
    int status = 0;

    while (1) {
        exp = 0;
        if (exprseq_length(&ctx->exprseq) != 0) {
            exp = exprseq_remhead(&ctx->exprseq);
        } else {
            lt = parser_next(pctx, QL_NORMAL, &lex);
            dfunc = lookup_dispatcher(ctx, lt);
            if (dfunc != 0) {
                exp = dfunc(ctx, lt, lex);
                if (exp != 0) {
                    lexeme_free(lex);
                }
            }
            if (exp == 0 && check_unary_op(lt, &op)) {
                lexeme_free(lex);
                exp = parse_op_expr(ctx, op, 0);
                if (exp == 0) {
                    break;
                }
            }
            if (exp == 0) {
                exp = parse_primary(ctx, lt, lex);
                if (exp == 0) {
                    parser_insert(pctx, lex);
                    break;
                }
            }
        }

        if (exp != 0) {
            if (expr_is_primary(exp)) {
                lt = parser_next(pctx, QL_NORMAL, &lex);
                op = lextype_to_optype(lt);
                if (op == OPER_NONE) {
                    parser_insert(pctx, lex);
                } else {
                    exp = parse_op_expr(ctx, op, exp);
                }
            }
            break;
        }
    }

    if (exp != 0 && expr_type(exp) == EXPTYPE_OPERATOR) {
        reduce_op_expr(ctx, &exp);
    }

    if (exp != 0) {
        *expp = exp;
        status = 1;
    }

    return status;

} /* parse_expr */

/*
 * expr_expr_next
 *
 * External interface to the expression parser.
 * Automatically binds literals and segments
 * to appropriate lexemes.
 */
int expr_expr_next (expr_ctx_t ctx, expr_node_t **expp)
{
    expr_node_t *exp;
    if (!parse_expr(ctx, &exp)) {
        return 0;
    }
    *expp = exp;
    return 1;
}

int
expr_parse_seq (expr_ctx_t ctx, lexseq_t *seq, expr_node_t **expp)
{
    parse_ctx_t pctx = ctx->pctx;
    strdesc_t nullstr = STRDEF("");
    int status;

    parser_insert(pctx, lexeme_create(LEXTYPE_MARKER, &nullstr));
    parser_insert_seq(pctx, seq);
    *expp = 0;
    status = parse_expr(ctx, expp);
    if (status) {
        status = parser_expect(pctx, QL_NORMAL, LEXTYPE_MARKER, 0, 1);
    }
    if (!status) {
        lexeme_t *lex;
        while (parser_next(pctx, QL_NORMAL, &lex) != LEXTYPE_MARKER) {
            lexseq_instail(seq, lex);
        }
    }
    return status;
}
/*
 * expr_parse_ctce
 *
 * Parses a compile-time constant expression.
 * Can be called in places where a CTCE is expected,
 * as well as by the %CTCE lexical function.
 *
 * If lexp is non-NULL, we assume the caller needs
 * a lexeme, even if we don't have a CTCE here, so
 * we signal an error and insert a zero value so that
 * parsing can continue.
 *
 * If lexp is NULL, this is just a call from %CTCE,
 * where we just need to test, and status can be zero.
 *
 * We always return zero if there is an error
 * parsing the expression. XXX
 */
int
expr_parse_ctce (expr_ctx_t ctx, lexeme_t **lexp)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *exp = 0;
    int status = 0;

    ctx->longstringsok = 1;
    if (!parse_expr(ctx, &exp)) {
        ctx->longstringsok = 0;
        return 0;
    }
    ctx->longstringsok = 0;

    if (expr_type(exp) == EXPTYPE_PRIM_LIT) {
        if (lexp != 0) {
            strdesc_t *str = string_printf(0, "%ld", expr_litval(exp));
            *lexp = parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str);
            lexeme_val_setsigned(*lexp, expr_litval(exp));
            (*lexp)->type = LEXTYPE_NUMERIC;
            string_free(str);
        }
        status = 1;
    } else {
        // not a CTCE
        if (lexp != 0) {
            strdesc_t dsc = STRDEF("0");
            /* XXX error condition */
            *lexp = parser_lexeme_create(pctx, LEXTYPE_NUMERIC, &dsc);
            lexeme_val_setsigned(*lexp, 0);
            (*lexp)->type = LEXTYPE_NUMERIC;
            status = 1;
        } else {
            status = 0;
        }
    }
    expr_node_free(ctx, exp);
    return status;

} /* expr_parse_ctce */

/*
 * parse_ltce
 *
 * Parses a link-time constant expression.
 * Can be called in places where an LTCE is expected,
 * as well as by the %LTCE lexical function.
 *
 * If lexp is non-NULL, we assume the caller needs
 * a lexeme, even if we don't have a LTCE here, so
 * we signal an error and insert a zero value so that
 * parsing can continue.
 *
 * If lexp is NULL, this is just a call from %LTCE,
 * where we just need to test, and status can be zero.
 *
 * We always return zero if there is an error
 * parsing the expression. XXX
 */
int
parse_ltce (expr_ctx_t ctx, expr_node_t **expp)
{
    expr_node_t *exp = 0;
    int status = 0;

    if (!parse_expr(ctx, &exp) || exp == 0) {
        return 0;
    }

    if (expr_is_ltce(exp)) {
        status = 1;
    }
    if (expp == 0) {
        expr_node_free(ctx, exp);
    } else {
        *expp = exp;
    }
    return status;

} /* parse_ltce */

/*
 * expr_parse_block
 *
 * External routine for parsing a block (specifically
 * for the MODULE declaration).  NB: no CODECOMMENT,
 * no labels.
 */
int
expr_parse_block (expr_ctx_t ctx, expr_node_t **blockexp)
{
    lextype_t lt;
    namereflist_t labels;

    namereflist_init(&labels);
    lt = parser_next(ctx->pctx, QL_NORMAL, 0);
    if (lt != LEXTYPE_DELIM_LPAR &&
        lt != LEXTYPE_EXP_DELIM_BEGIN) {
        /* XXX error condition */
        return 0;
    }

    return parse_block(ctx, lt, blockexp, 0, &labels);

} /* expr_parse_block */


/*
 * parse_condexp
 *
 * IF test THEN consequent {ELSE alternative}
 */
static expr_node_t *
parse_condexp (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    expr_node_t *test, *cons, *alt, *exp;
    parse_ctx_t pctx = ctx->pctx;

    test = cons = alt = 0;
    if (!parse_expr(ctx, &test)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_THEN, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!parse_expr(ctx, &cons)) {
        /* XXX error condition */
        return 0;
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_ELSE, 0, 1)) {
        if (!parse_expr(ctx, &alt)) {
            /* XXX error condition */
        }
        return 0;
    }

    // Optimize away constant tests
    if (expr_type(test) == EXPTYPE_PRIM_LIT) {
        if (expr_litval(test) & 1) {
            exp = cons;
        } else {
            exp = (alt == 0 ?
                   expr_node_alloc(ctx, EXPTYPE_NOOP, parser_curpos(pctx)) :
                   alt);
            expr_node_free(ctx, cons);
        }
        expr_node_free(ctx, test);
    } else {
        exp = expr_node_alloc(ctx, EXPTYPE_CTRL_COND, parser_curpos(pctx));
        expr_cond_test_set(exp, test);
        expr_cond_consequent_set(exp, cons);
        expr_cond_alternative_set(exp, alt);
        expr_has_value_set(exp, (alt != 0));
    }

    return exp;

} /* parse_condexp */

static int
get_ctce (expr_ctx_t ctx, long *valp)
{
    expr_node_t *exp = 0;

    ctx->longstringsok = 1;
    if (!parse_expr(ctx, &exp)) {
        ctx->longstringsok = 0;
        return 0;
    }
    ctx->longstringsok = 0;
    if (expr_type(exp) == EXPTYPE_PRIM_LIT) {
        *valp = expr_litval(exp);
        expr_node_free(ctx, exp);
        return 1;
    }
    expr_node_free(ctx, exp);
    return 0;

} /* get_ctce */

initval_t *
expr_initval_add (expr_ctx_t ctx, initval_t *ivlist, expr_node_t *exp,
                  unsigned int width)
{
    stgctx_t stg = expr_stg_ctx(ctx);
    long val;

    switch (expr_type(exp)) {
        case EXPTYPE_PRIM_LIT:
            val = expr_litval(exp);
            expr_node_free(ctx, exp);
            return initval_scalar_add(stg, ivlist, 1, val, width, 0);
        default:
            return initval_expr_add(stg, ivlist, 1, 1, exp, width, 0);
            break;
    }
}

/*
 * parse_case
 *
 * CASE exp FROM ctce TO ctce OF
 * SET
 *  [ctce { TO ctce } | INRANGE | OUTRANGE,...]: expression;
 * TES
 */
static expr_node_t *
parse_case (expr_ctx_t ctx, lextype_t lt, lexeme_t *curlex)
{
    
    expr_node_t *caseidx = 0;
    expr_node_t **cases, **unique, *exp;
    expr_node_t *outrange = 0;
    int *todo;
    parse_ctx_t pctx = ctx->pctx;
    lexeme_t *lex;
    long lo, hi, ncases, i;
    int saw_inrange, saw_outrange, status;
    int unique_actions;
    int every_case_has_value = 1;

    if (!parse_expr(ctx, &caseidx)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!get_ctce(ctx, &lo)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!get_ctce(ctx, &hi)) {
        /* XXX error condition */
        return 0;
    }
    if (lo > hi) {
        long tmp;
        /* XXX error condition */
        tmp = lo;
        lo = hi;
        hi = tmp;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    ncases = (hi - lo) + 1;
    cases = malloc(ncases*sizeof(expr_node_t *));
    unique = malloc(ncases*sizeof(expr_node_t *));
    memset(cases, 0, ncases*sizeof(expr_node_t *));
    memset(unique, 0, ncases*sizeof(expr_node_t *));
    todo = malloc(ncases*sizeof(int));
    saw_inrange = saw_outrange = 0;
    unique_actions = 0;
    status = 1;
    while (1) {
        int is_outrange = 0;
        memset(todo, 0, sizeof(int)*ncases);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_KWD_TES);
            status = 0;
            break;
        }
        while (1) {
            lt = parser_next(pctx, QL_NORMAL, &lex);
            if (lt == LEXTYPE_KWD_INRANGE) {
                if (saw_inrange) {
                    /* XXX error condition */
                }
                saw_inrange = 1;
                for (i = 0; i < ncases; i++) {
                    todo[i] = (cases[i] == 0);
                }
            } else if (lt == LEXTYPE_KWD_OUTRANGE) {
                if (saw_outrange) {
                    /* XXX error condition */
                }
                is_outrange = saw_outrange = 1;
            } else if (lt == LEXTYPE_NUMERIC) {
                long beginval = lexeme_signedval(lex);
                long endval = beginval;
                lexeme_free(lex);
                if (saw_inrange) {
                    /* XXX error condition */
                }
                if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
                    lt = parser_next(pctx, QL_NORMAL, &lex);
                    if (lt != LEXTYPE_NUMERIC) {
                        /* XXX error condition */
                    } else {
                        endval = lexeme_signedval(lex);
                        if (beginval > endval) {
                            long tmp = beginval;
                            /* XXX error condition */
                            beginval = endval;
                            endval = tmp;
                        }
                    }
                    lexeme_free(lex);
                }
                for (i = beginval-lo; i <= endval-lo; todo[i++] = 1);
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
                break;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                /* XXX error condition */
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RBRACK);
                break;
            }
        } /* per-case while */
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition, but keep trying */
        }
        exp = 0;
        if (!parse_expr(ctx, &exp)) {
            /* XXX error condition */
            status = 0;
            break;
        }
        if (!expr_has_value(exp)) {
            every_case_has_value = 0;
        }
        if (is_outrange) {
            if (outrange != 0) {
                /* XXX error condition - repeats not allowed */
            }
            outrange = exp;
        } else {
            unique[unique_actions++] = exp;
        }
        for (i = 0; i < ncases; i++) {
            if (todo[i]) {
                if (cases[i] != 0) {
                    /* XXX error condition - repeats not allowed */
                }
                cases[i] = exp;
            }
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            /* XXX error condition, but keep going */
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
            break;
        }

    } /* outer while */

    if (status) {
        for (i = 0; i < ncases && cases[i] != 0; i++);
        if (i < ncases) {
            /* XXX error condition - not all cases covered */
            status = 0;
        }
    }

    if (status) {
        exp = expr_node_alloc(ctx, EXPTYPE_CTRL_CASE, parser_curpos(pctx));
        expr_case_bounds_set(exp, lo, hi);
        expr_case_outrange_set(exp, outrange);
        expr_case_actions_set(exp, cases);
        expr_case_index_set(exp, caseidx);
        expr_has_value_set(exp, every_case_has_value);
    } else {
        expr_node_free(ctx, outrange);
        expr_node_free(ctx, caseidx);
        free(cases);
        while (unique_actions > 0) {
            expr_node_free(ctx, unique[--unique_actions]);
        }
    }

    free(unique);
    free(todo);

    return (status ? exp : 0);

} /* parse_case */
/*
 * parse_select
 *
 * SELECT{A|U}|SELECTONE{A|U} exp OF
 * SET
 *  [ expr { TO expr } | OTHERWISE | ALWAYS,...]: expr;
 * TES
 */
static expr_node_t *
parse_select (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *si, *exp, *lo, *hi;
    expr_node_t *selectors, *sellast, *sel;
    exprseq_t selseq;
    int every_selector_has_value = 1;
    int is_selectone;
    int need_default_otherwise = 1;

    si = 0;
    if (!parse_expr(ctx, &si)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        /* XXX error condition */
    }
    exprseq_init(&selseq);
    is_selectone = (curlt == LEXTYPE_CTRL_SELECTONE
                    || curlt == LEXTYPE_CTRL_SELECTONEU
                    || curlt == LEXTYPE_CTRL_SELECTONEA);
    while (1) {
        selectors = sellast = 0;
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TES, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_KWD_TES);
            break;
        }
        sel = expr_node_alloc(ctx, EXPTYPE_SELECTOR, parser_curpos(pctx));
        while (1) {
            lo = hi = 0;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OTHERWISE, 0, 1)) {
                expr_selector_otherwise_set(sel, 1);
                need_default_otherwise = 0;
            } else if (!is_selectone &&
                       parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_ALWAYS, 0, 1)) {
                expr_selector_always_set(sel, 1);
                need_default_otherwise = 0;
            } else if (parse_expr(ctx, &lo)) {
                if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
                    if (!parse_expr(ctx, &hi)) {
                        /* XXX error condition */
                    }
                }
                expr_selector_lohi_set(sel, lo, hi);
            } else {
                /* XXX error condition */
                expr_node_free(ctx, sel);
                expr_node_free(ctx, selectors);
                selectors = 0;
                break;
            }
            if (selectors == 0) {
                selectors = sellast = sel;
            } else {
                expr_selector_next_set(sellast, sel);
                sellast = sel;
            }
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RBRACK, 0, 1)) {
                break;
            }
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
                /* XXX error condition */
            }
        } /* inner loop */
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition */
        }
        exp = 0;
        if (!parse_expr(ctx, &exp)) {
            /* XXX error condition */
        }
        if (selectors == 0) {
            expr_node_free(ctx, exp);
        } else {
            expr_selector_action_set(selectors, exp);
            if (!expr_has_value(exp)) {
                every_selector_has_value = 0;
            }
            exprseq_instail(&selseq, selectors);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            /* XXX error condition */
        }
    } /* outer loop */

    // Spec says that if nothing matches, the value of the
    // expression should be set to -1, so fill in a default
    // OTHERWISE here to do that, if we haven't seen an OTHERWISE
    // or ALWAYS.
    if (need_default_otherwise) {
        sel = expr_node_alloc(ctx, EXPTYPE_SELECTOR, parser_curpos(pctx));
        expr_selector_otherwise_set(sel, 1);
        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
        expr_litval_set(exp, -1);
        expr_selector_action_set(sel, exp);
        exprseq_instail(&selseq, sel);
    }

    if (exprseq_length(&selseq) == 0) {
        /* XXX error condition */
        expr_node_free(ctx, si);
        return 0;
    }

    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_SELECT, parser_curpos(pctx));
    expr_sel_index_set(exp, si);
    expr_sel_oneonly_set(exp, is_selectone);
    switch (curlt) {
        case LEXTYPE_CTRL_SELECT:
        case LEXTYPE_CTRL_SELECTONE:
            expr_sel_cmptype_set(exp, OPER_CMP_EQL);
            break;
        case LEXTYPE_CTRL_SELECTU:
        case LEXTYPE_CTRL_SELECTONEU:
            expr_sel_cmptype_set(exp, OPER_CMP_EQLU);
            break;
        case LEXTYPE_CTRL_SELECTA:
        case LEXTYPE_CTRL_SELECTONEA:
            expr_sel_cmptype_set(exp, OPER_CMP_EQLA);
            break;
        default:
            break;
    }
    exprseq_set(expr_sel_selectors(exp), &selseq);
    expr_has_value_set(exp, every_selector_has_value);

    return exp;
}
/*
 * parse_incrdecr
 *
 * INCR{A|U}|DECR{A|U} name {FROM exp} {TO exp} {BY exp}
 */
static expr_node_t *
parse_incrdecr (expr_ctx_t ctx, lextype_t curlt, lexeme_t *curlex)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *fromexp, *toexp, *byexp, *body, *exp;
    strdesc_t *indexname;
    name_t *np;
    textpos_t pos;
    scopectx_t scope;

    fromexp = toexp = byexp = 0;
    scope = parser_scope_begin(pctx);
    if (!parse_decl_name(pctx, scope, &indexname, &pos)) {
        /* XXX error condition */
        parser_scope_end(pctx);
        return 0;
    }

    // Implicit declaration of the index name as LOCAL
    np = datasym_declare(scope, indexname, SYMSCOPE_LOCAL, 0, pos);
    if (np == 0) {
        /* XXX error condition */
        parser_scope_end(pctx);
        return 0;
    }

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        if (!parse_expr(ctx, &fromexp)) {
            /* XXX error condition */
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        if (!parse_expr(ctx, &toexp)) {
            /* XXX error condition */
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_BY, 0, 1)) {
        if (!parse_expr(ctx, &byexp)) {
            /* XXX error condition */
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        /* XXX error condition */
    }
    body = 0;
    ctx->loopdepth += 1;
    if (!parse_expr(ctx, &body)) {
        ctx->loopdepth -= 1;
        parser_loopdepth_decr(pctx);
        expr_node_free(ctx, fromexp);
        expr_node_free(ctx, toexp);
        expr_node_free(ctx, byexp);
        parser_scope_end(pctx);
        return 0;
    }
    ctx->loopdepth += 1;
    parser_scope_end(pctx);
    exp = expr_node_alloc(ctx, EXPTYPE_CTRL_LOOPID, parser_curpos(pctx));
    expr_idloop_scope_set(exp, scope);
    expr_idloop_init_set(exp, fromexp);
    expr_idloop_term_set(exp, toexp);
    expr_idloop_step_set(exp, byexp);
    expr_idloop_body_set(exp, body);
    if (curlt == LEXTYPE_CTRL_DECR ||
        curlt == LEXTYPE_CTRL_DECRA ||
        curlt == LEXTYPE_CTRL_DECRU) {
        expr_idloop_decr_set(exp, 1);
    }
    switch (curlt) {
        case LEXTYPE_CTRL_DECR:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQ);
            break;
        case LEXTYPE_CTRL_INCR:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQ);
            break;
        case LEXTYPE_CTRL_DECRU:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQU);
            break;
        case LEXTYPE_CTRL_INCRU:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQU);
            break;
        case LEXTYPE_CTRL_DECRA:
            expr_idloop_cmptype_set(exp, OPER_CMP_GEQA);
            break;
        case LEXTYPE_CTRL_INCRA:
            expr_idloop_cmptype_set(exp, OPER_CMP_LEQA);
            break;
        default:
            break;
    }
    // XXX Really should validate that all exits have values,
    //     but not sure we can do this until a later processing stage
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_incrdecr */

int
expr_parse_ISSTRING (expr_ctx_t ctx, int *allstrp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    expr_node_t *exp;
    int allstr = 1;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    ctx->longstringsok = 1;
    while (1) {
        if (!parse_expr(ctx, &exp)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            allstr = 0;
            break;
        }
        if (expr_type(exp) != EXPTYPE_PRIM_LIT || expr_litstring(exp) == 0) {
            allstr = 0;
        }
        expr_node_free(ctx, exp);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
        }
    }

    ctx->longstringsok = 0;
    *allstrp = allstr;
    return 1;

} /* expr_parse_ISSTRING */

int
expr_parse_xCTE (expr_ctx_t ctx, int checkltce, int *allokp)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    expr_node_t *exp;
    int allok = 1;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    ctx->longstringsok = 1;
    while (1) {
        if (!parse_expr(ctx, &exp)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            allok = 0;
            break;
        }
        if (checkltce == 0) {
            if (expr_type(exp) != EXPTYPE_PRIM_LIT) {
                allok = 0;
            }
        } else {
            if (!expr_is_ltce(exp)) {
                allok = 0;
            }
        }
        expr_node_free(ctx, exp);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            /* XXX error condition */
        }
    }
    ctx->longstringsok = 0;
    *allokp = allok;
    return 1;

} /* expr_parse_xCTE */

int
expr_get_allocation (expr_ctx_t ctx, strdesc_t *name, unsigned int *units)
{
    name_t *np;
    data_attr_t attr;
    parse_ctx_t pctx = expr_parse_ctx(ctx);

    np = datasym_search(parser_scope_get(pctx), name, &attr);
    if (np == 0) {
        return 0;
    }
    *units = attr.units;
    return 1;

} /* expr_get_allocation */

int
expr_parse_SIZE (expr_ctx_t ctx, unsigned int *units)
{
    parse_ctx_t pctx = expr_parse_ctx(ctx);
    lexeme_t *lex;
    strudef_t *stru;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    *units = 0;
    parser_set_indecl(pctx, 1);
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_STRUCTURE, &lex, 1)) {
        /* XXX error condition */
        return 1;
    }
    parser_set_indecl(pctx, 0);
    if (!structure_allocate(ctx, lexeme_ctx_get(lex), &stru, units, 0)) {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        /* XXX error condition */
    }
    
    return 1;

} /* expr_parse_SIZE */

