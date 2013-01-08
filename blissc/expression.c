/*
 *++
 *	File:			expression.c
 *
 *	Abstract:		Expression parsing
 *
 *  Module description:
 *		This module is the center of expression handling.  It
 *		sits above the lexical-processing layer (the parser module
 *		and friends), and cooperates with the symbol, storage, and
 *		declaration modules to build the equivalent of an Abstract
 *		Syntax Tree (AST) for a module.
 *
 *		Expressions are represented by the expr_node_t type, which
 *		holds exactly one expression of any valid expression type.
 *		Depending on the type, a given expression node may reference
 *		one or more other expressions; for example, a block expression
 *		contains an exprseq_t listing the expressions contained in
 *		the block.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		22-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdlib.h>
#include <stdio.h>
#include "expression.h"
#include "gencode.h"
#include "execfuncs.h"
#include "declarations.h"
#include "symbols.h"
#include "parser.h"
#include "listings.h"
#include "nametable.h"
#include "lexeme.h"
#include "machinedef.h"
#include "logging.h"

#undef DOEXPTYPE
#define DOEXPTYPE(t_) "EXPTYPE_" #t_,
static const char *exptypenames[] = { DOEXPTYPES };
#undef DOEXPTYPE

struct extenthdr_s {
    struct extenthdr_s *next;
};

struct expr_ctx_s {
    lstgctx_t           lstgctx;
    gencodectx_t        gctx;
    strctx_t            strctx;
    parse_ctx_t         pctx;
    namectx_t           namectx;
    lexctx_t            lctx;
    machinedef_t       *mach;
    logctx_t            logctx;
    symctx_t            symctx;
    struct extenthdr_s *extents;
    expr_node_t        *freenodes;
    expr_dispatch_fn    dispatchers[LEXTYPE_EXPKWD_MAX-LEXTYPE_EXPKWD_MIN+1];
    expr_dispatch_fn    name_dispatchers[LEXTYPE_NAME_MAX-LEXTYPE_NAME_MIN+1];
    namereflist_t       routinestack;
    void               *fake_label_ptr;
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
    NAMEDEF("WITH", LEXTYPE_KWD_WITH, NAME_M_RESERVED),
    NAMEDEF("%REF", LEXTYPE_KWD_PCTREF, NAME_M_RESERVED),
    NAMEDEF("RETURN", LEXTYPE_CTRL_RETURN, NAME_M_RESERVED),
    NAMEDEF("OTHERWISE", LEXTYPE_KWD_OTHERWISE, NAME_M_RESERVED),
    NAMEDEF("ALWAYS", LEXTYPE_KWD_ALWAYS, NAME_M_RESERVED),
    NAMEDEF("%CTCE", LEXTYPE_LXF_CTCE, NAME_M_RESERVED),
    NAMEDEF("%LTCE", LEXTYPE_LXF_LTCE, NAME_M_RESERVED),
    NAMEDEF("%EXACTSTRING", LEXTYPE_LXF_EXACTSTRING, NAME_M_RESERVED),
    NAMEDEF("%CHAR", LEXTYPE_LXF_CHAR, NAME_M_RESERVED),
    NAMEDEF("%IF", LEXTYPE_LXF_IF, NAME_M_RESERVED),
    NAMEDEF("%ISSTRING", LEXTYPE_LXF_ISSTRING, NAME_M_RESERVED),
    NAMEDEF("%NUMBER", LEXTYPE_LXF_NUMBER, NAME_M_RESERVED),
    NAMEDEF("%NBITS", LEXTYPE_LXF_NBITS, NAME_M_RESERVED),
    NAMEDEF("%NBITSU", LEXTYPE_LXF_NBITSU, NAME_M_RESERVED),
    NAMEDEF("%ALLOCATION", LEXTYPE_LXF_ALLOCATION, NAME_M_RESERVED)
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

static strdesc_t one = STRDEF("1"), zero = STRDEF("0"), nullstr = STRDEF("");

static expr_node_t *parse_op_expr(expr_ctx_t ctx, optype_t op,
                                  expr_node_t *lhs, expr_node_t *rhs);
static expr_node_t *parse_codecomment(expr_ctx_t ctx, lextype_t lt, lexeme_t *lex);
static int parse_xTCE(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_CHAR(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_EXACTSTRING(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_IF(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_ISSTRING(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_NUMBER(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_nbits_func(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static int parse_ALLOCATION(parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt);
static void reduce_op_expr(expr_ctx_t ctx, expr_node_t **expp);

// Defined in expr_control.c
void expr_control_init(expr_ctx_t ctx);

/*
 * -- Miscellaneous utility functions --
 */

/*
 * expr_signal
 *
 * Convenience function for calling log_signal.
 */
void
expr_signal (expr_ctx_t ctx, statcode_t code, ...)
{
    va_list ap;
    va_start(ap, code);
    log_vsignal(ctx->logctx, parser_curpos(ctx->pctx), code, ap);
    va_end(ap);

} /* expr_signal */

/*
 * Type name functions - for debugging
 */
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

int oper_is_unary (optype_t op) { return (operinfo[op].isr2l & 2) != 0; }
static int op_is_r2l (optype_t op) { return (operinfo[op].isr2l & 1) != 0; }
static int op_priority (optype_t op) { return operinfo[op].priority; }

/*
 * check_unary_op
 *
 * If a lextype is for a unary operator (+, -, NOT), returns
 * 1 and maps the lextype to the operator type.
 */
static int
check_unary_op (lextype_t lt, optype_t *opp)
{
    if (lt == LEXTYPE_OP_ADD || lt == LEXTYPE_OP_SUB ||
        lt == LEXTYPE_OP_FETCH) {
        *opp = (lt == LEXTYPE_OP_ADD ? OPER_UNARY_PLUS
                : (lt == LEXTYPE_OP_SUB ? OPER_UNARY_MINUS : OPER_FETCH));
        return 1;
    } else if (lt == LEXTYPE_OP_NOT) {
        *opp = OPER_NOT;
        return 1;
    }
    return 0;

} /* check_unary_op */

/*
 * expr_node_alloc
 *
 * Allocates an expression node.
 */
expr_node_t *
expr_node_alloc (expr_ctx_t ctx, exprtype_t type, textpos_t pos)
{
    expr_node_t *node;
    int i;

    if (ctx->freenodes == 0) {
        struct extenthdr_s *extent;
        extent = malloc(sizeof(struct extenthdr_s) + (ALLOC_QTY * sizeof(expr_node_t)));
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freenodes = (expr_node_t *)(extent + 1);
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

/*
 * expr_node_free
 *
 * Frees an expression node and any expressions the
 * node references.
 */
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
            expr_node_t **actarray = expr_case_actions(node);
            long *cases = expr_case_cases(node);
            long i, actcount = expr_case_actioncount(node);
            expr_node_free(ctx, expr_case_index(node));
            for (i = 0; i < actcount; i++) {
                expr_node_free(ctx, actarray[i]);
            }
            free(actarray);
            free(cases);
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
                memset(sel, 0x77, sizeof(expr_node_t));
                expr_next_set(sel, ctx->freenodes);
                ctx->freenodes = sel;
                sel = subnext;
            } while (sel != 0);

            break;
        }
        case EXPTYPE_CTRL_SELECT:
            expr_node_free(ctx, expr_sel_index(node));
            exprseq_free(ctx, expr_sel_selectors(node));
            break;

        case EXPTYPE_CTRL_EXIT:
        case EXPTYPE_CTRL_RET:
            expr_node_free(ctx, expr_exit_value(node));
            break;

        case EXPTYPE_PRIM_LIT:
            string_free(ctx->strctx, expr_litstring(node));
            break;

        case EXPTYPE_EXECFUN:
            exprseq_free(ctx, expr_func_arglist(node));
            break;

        case EXPTYPE_NOOP:
        case EXPTYPE_PRIM_SEG:
            break;
    }
    memset(node, 0x77, sizeof(expr_node_t));
    expr_next_set(node, ctx->freenodes);
    ctx->freenodes = node;

} /* expr_node_free */

/*
 * expr_node_copy
 *
 * Copy-constructor for an expression node, which copies
 * the node's contents.
 */
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
            expr_node_t **actarray = expr_case_actions(node);
            expr_node_t **dstarray;
            long *casearray, *casedst;
            long i, actcount, lo, hi;
            expr_case_index_set(dst, expr_node_copy(ctx, expr_case_index(node)));
            expr_case_outrange_set(dst, expr_case_outrange(node));
            lo = expr_case_lowbound(node);
            hi = expr_case_highbound(node);
            expr_case_bounds_set(dst, lo, hi);
            expr_case_outrange_set(dst, expr_case_outrange(node));
            actcount = expr_case_actioncount(node);
            dstarray = malloc(actcount*sizeof(expr_node_t *));
            for (i = 0; i < actcount; i++) {
                dstarray[i] = expr_node_copy(ctx, actarray[i]);
            }
            expr_case_actions_set(dst, actcount, dstarray);
            casearray = expr_case_cases(node);
            casedst = malloc((hi-lo+1)*sizeof(long));
            memcpy(casedst, casearray, (hi-lo+1)*sizeof(long));
            expr_case_cases_set(dst, casedst);
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
        case EXPTYPE_CTRL_RET:
            expr_exit_value_set(dst, expr_node_copy(ctx, expr_exit_value(node)));
            expr_exit_label_set(dst, expr_exit_label(node));
            break;

        case EXPTYPE_PRIM_LIT:
            if (expr_litstring(node) != 0) {
                expr_litstring_set(dst, string_copy(ctx->strctx, 0, expr_litstring(node)));
            }
            break;

        case EXPTYPE_EXECFUN:
            exprseq_copy(ctx, expr_func_arglist(dst),
                         expr_func_arglist(node));
            break;

        case EXPTYPE_NOOP:
        case EXPTYPE_PRIM_SEG:
            break;
    }

    return dst;

} /* expr_node_copy */

/*
 * exprseq_free
 *
 * Frees a sequence of expressions.
 */
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

/*
 * exprseq_copy
 *
 * Copies an expression sequence.
 */
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

/*
 * lextype_to_optype
 *
 * Translates a lexeme type to an operator type,
 * or OPER_NONE if the lextype does not map to an operator.
 */
static optype_t
lextype_to_optype (lextype_t lt)
{
    if (lt < LEXTYPE_OP_MIN || lt > LEXTYPE_OP_MAX) {
        return OPER_NONE;
    }
    return opmap[lt-LEXTYPE_OP_MIN];

} /* lextype_to_optype */

/*
 * parse_plit
 *
 * Calls define_plit to parse a PLIT or UPLIT, then wraps the
 * resulting data segment (which is given a tempname) into an
 * expression node.
 */
static expr_node_t *
parse_plit (expr_ctx_t ctx, lextype_t curlt, lexeme_t *lex)
{
    parse_ctx_t pctx = ctx->pctx;
    name_t *plitname;
    expr_node_t *exp;

    plitname = define_plit(ctx, curlt, parser_curpos(pctx));
    if (plitname == 0) {
        return 0;
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, parser_curpos(pctx));
    expr_seg_name_set(exp, plitname);
    expr_is_ltce_set(exp, 1);
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_plit */

/*
 * expr_dispatch_register
 *
 * Registers a handler for a NAME lextype or an expression keyword
 * type.  Other modules call this routine to register their handlers,
 * which get invoked from expr_parse().
 */
void
expr_dispatch_register (expr_ctx_t ctx, lextype_t lt, expr_dispatch_fn fn)
{
    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        ctx->name_dispatchers[lt-LEXTYPE_NAME_MIN] = fn;
        return;
    }
    if (lt < LEXTYPE_EXPKWD_MIN || lt > LEXTYPE_EXPKWD_MAX) {
        expr_signal(ctx, STC__INTCMPERR, "expr_dispatch_register");
        return;
    }
    ctx->dispatchers[lt-LEXTYPE_EXPKWD_MIN] = fn;

} /* expr_dispatch_register */

/*
 * lookup_dispatcher
 *
 * Finds the dispatcher for a given lexeme type.
 */
static expr_dispatch_fn
lookup_dispatcher (expr_ctx_t ctx, lextype_t lt)
{
    if (lt >= LEXTYPE_NAME_MIN && lt <= LEXTYPE_NAME_MAX) {
        return ctx->name_dispatchers[lt-LEXTYPE_NAME_MIN];
    }
    if (lt >= LEXTYPE_EXPKWD_MIN && lt <= LEXTYPE_EXPKWD_MAX) {
        return ctx->dispatchers[lt-LEXTYPE_EXPKWD_MIN];
    }
    return 0;

} /* lookup_dispatcher */

/*
 * expr_init
 *
 * Module initialization.
 */
expr_ctx_t
expr_init (strctx_t strctx, parse_ctx_t pctx, scopectx_t kwdscope)
{
    expr_ctx_t ectx;
    int i;

    ectx = malloc(sizeof(struct expr_ctx_s));
    if (ectx == 0) {
        return 0;
    }
    memset(ectx, 0, sizeof(struct expr_ctx_s));
    ectx->strctx = strctx;
    ectx->pctx = pctx;
    ectx->lctx = parser_lexmemctx(pctx);
    ectx->mach = parser_get_machinedef(pctx);
    ectx->namectx = scope_namectx(kwdscope);
    ectx->logctx = parser_logctx(pctx);
    ectx->lstgctx = parser_lstgctx(pctx);
    ectx->fake_label_ptr = (void *)0xffeeeeff;
    ectx->symctx = symbols_init(ectx);
    ectx->gctx = gencode_init(ectx, ectx->logctx, ectx->mach, ectx->symctx);
    symbols_connect_hooks(ectx->symctx);


    for (i = 0; i < sizeof(expr_names)/sizeof(expr_names[0]); i++) {
        name_declare(kwdscope, &expr_names[i], 0, 0, 0, 0);
    }

    expr_dispatch_register(ectx, LEXTYPE_KWD_PLIT, parse_plit);
    expr_dispatch_register(ectx, LEXTYPE_KWD_UPLIT, parse_plit);
    expr_dispatch_register(ectx, LEXTYPE_KWD_CODECOMMENT, parse_codecomment);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_CTCE, parse_xTCE);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_LTCE, parse_xTCE);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_CHAR, parse_CHAR);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_EXACTSTRING, parse_EXACTSTRING);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_ISSTRING, parse_ISSTRING);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_IF, parse_IF);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_NUMBER, parse_NUMBER);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_NBITS, parse_nbits_func);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_NBITSU, parse_nbits_func);
    parser_lexfunc_register(pctx, ectx, LEXTYPE_LXF_ALLOCATION, parse_ALLOCATION);

    expr_control_init(ectx);
    declarations_init(ectx, pctx, kwdscope, ectx->mach);
    execfunc_init(ectx, kwdscope);

    return ectx;

} /* expr_init */

/*
 * Getter/setters for the expression context
 */
parse_ctx_t expr_parse_ctx (expr_ctx_t ctx) { return ctx->pctx; }
namectx_t expr_namectx (expr_ctx_t ctx) { return ctx->namectx; }
machinedef_t *expr_machinedef (expr_ctx_t ctx) { return ctx->mach; }
lexctx_t expr_lexmemctx (expr_ctx_t ctx) { return ctx->lctx; }
logctx_t expr_logctx (expr_ctx_t ctx) { return ctx->logctx; }
void expr_loopdepth_incr (expr_ctx_t ctx) { ctx->loopdepth += 1; }
void expr_loopdepth_decr (expr_ctx_t ctx) { ctx->loopdepth -= 1; }
int expr_loopdepth_get (expr_ctx_t ctx) { return ctx->loopdepth; }
void *expr_fake_label_ptr (expr_ctx_t ctx) { return ctx->fake_label_ptr; }
strctx_t expr_strctx(expr_ctx_t ctx) { return ctx->strctx; }
lstgctx_t expr_lstgctx(expr_ctx_t ctx) { return ctx->lstgctx; }
void *expr_symctx(expr_ctx_t ctx) { return ctx->symctx; }
void *expr_gencodectx(expr_ctx_t ctx) { return ctx->gctx; }

void
expr_finish (expr_ctx_t ctx)
{
    struct extenthdr_s *e, *enext;

    if (ctx == 0) {
        return;
    }

    parser_finish(ctx->pctx);

    for (e = ctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }

    free(ctx);

} /* expr_finish */

/*
 * expr_push_routine
 *
 * Pushes a routine on the routine stack.  Provides a mechanism
 * for walking back through nested routine definitions.
 */
void
expr_push_routine (expr_ctx_t ctx, name_t *np)
{
    nameref_t *ref = nameref_alloc(ctx->namectx, np);
    if (ref == 0) {
        expr_signal(ctx, STC__INTCMPERR, "expr_push_routine");
    } else {
        namereflist_inshead(&ctx->routinestack, ref);
    }

} /* expr_push_routine */

/*
 * expr_pop_routine
 *
 * Pops a routine off the routine stack.
 */
void
expr_pop_routine (expr_ctx_t ctx)
{
    nameref_t *ref = namereflist_remhead(&ctx->routinestack);
    if (ref == 0) {
        expr_signal(ctx, STC__INTCMPERR, "expr_pop_routine");
    } else {
        nameref_free(ctx->namectx, ref);
    }

} /* expr_pop_routine */

/*
 * expr_current_routine
 *
 * Returns a pointer to the name of the routine
 * currently being parsed (or NULL if there is none).
 */
name_t *
expr_current_routine (expr_ctx_t ctx)
{
    nameref_t * ref = namereflist_head(&ctx->routinestack);
    if (ref == 0) {
        return 0;
    }
    return ref->np;

} /* expr_current_routine */

/*
 * parse_block
 *
 * Parses a block from its opener (either BEGIN or left
 * parenthesis).  The caller is expected to pass in the
 * CODECOMMENT string and list of labels, if they were
 * present.
 *
 * { CODECOMMENT 'qstring'{,...}: }
 * {label: ...}
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
    namereflist_t *labellist;
    textpos_t endpos;
    lextype_t closer = (curlt == LEXTYPE_EXP_DELIM_BEGIN ?
                        LEXTYPE_EXP_DELIM_END : LEXTYPE_DELIM_RPAR);

    // Set a special value for the labels so that
    // LEAVE processing will know that the labels are referring
    // to a block we are currently in the middle of parsing
    for (ref = namereflist_head(labels); ref != 0; ref = ref->tq_next) {
        if (ref->np != 0 && name_value_pointer(ref->np) == 0) {
            label_block_set(ref->np, ctx->fake_label_ptr);
        }
    }

    listing_newblock(ctx->lstgctx);
    parser_punctclass_set(pctx, PUNCT_SEMISEP_NOGROUP, LEXTYPE_DELIM_SEMI);
    lt = parser_next(pctx, QL_NORMAL, &lex);

    exprseq_init(&seq);
    valexp = 0;
    while (1) {
        if (lt == closer) {
            endpos = parser_curpos(pctx);
            lexeme_free(ctx->lctx, lex);
            break;
        } else if (lt >= LEXTYPE_DCL_MIN && lt <= LEXTYPE_DCL_MAX) {
			// Declarations are allowed only if we haven't
			// yet seen an expressions in the block
            if (exprseq_length(&seq) != 0) {
                expr_signal(ctx, STC__INTCMPERR, "parse_block");
                lexeme_free(ctx->lctx, lex);
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
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
            parser_skip_to_delim(pctx, closer);
            endpos = parser_curpos(pctx);
            break;
        }
        // The last expression in a block is its value
        // (as far as we know at compile time)
        valexp = exp;
        exprseq_instail(&seq, exp);
        lt = parser_next(pctx, QL_NORMAL, &lex);
        // Semicolon separator also discards the value
        if (lt == LEXTYPE_DELIM_SEMI) {
            valexp = 0;
            lexeme_free(ctx->lctx, lex);
            lt = parser_next(pctx, QL_NORMAL, &lex);
        }
    }

    listing_endblock(ctx->lstgctx, scope);
	// Check for the degenerate cases:
	//	1. empty block ==  NOOP
	//	2. block with single primary expression
	//  along with no labels, no declarations, and no codecomment
	//  In these cases, we can eliminate the block and just
	//  return the NOOP or primary expression.
    if (scope == 0 && codecomment == 0 && namereflist_empty(labels)) {
        if (exprseq_length(&seq) == 0) {
            *expp = expr_node_alloc(ctx, EXPTYPE_NOOP, endpos);
            return 1;
        }
        if (exprseq_length(&seq) == 1) {
            expr_node_t *exp = exprseq_remhead(&seq);
            if (expr_type(exp) == EXPTYPE_OPERATOR) {
                reduce_op_expr(ctx, &exp);
            }
            if (expr_is_primary(exp)) {
                *expp = exp;
                return 1;
            }
            exprseq_inshead(&seq, exp);
        }
    }
    if (scope != 0) {
        parser_scope_pop(pctx);
        sym_check_dangling_forwards(scope, parser_curpos(pctx));
    }
    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_BLK, endpos);
    expr_blk_scope_set(exp, scope);

    exprseq_append(expr_blk_seq(exp), &seq);
    expr_blk_valexp_set(exp, valexp);
    expr_has_value_set(exp, (valexp != 0));
    expr_blk_codecomment_set(exp, codecomment);
    labellist = expr_blk_labels(exp);
    *expp = exp;
    // Now point the labels at us
    while ((ref = namereflist_remhead(labels))) {
        if (ref->np == 0 || label_block(ref->np) != ctx->fake_label_ptr) {
            expr_signal(ctx, STC__INTCMPERR, "parse_block[2]");
        }
        label_block_set(ref->np, exp);
        namereflist_instail(labellist, ref);
    }

    return 1;

} /* parse_block */

/*
 * expr_parse_arglist
 *
 * Parses the actual parameters for a routine call (ordinary
 * or general).
 *
 * NB: assumes that we've just seen the opener - left paren
 * for ordinary calls, routine-address (but NOT the comma)
 * for general calls
 *
 * rtn is the routine-designator expression
 *
 * If successful, a RTNCALL expression node is returned.
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
        	// Handle %REF() calls - storing the expression and pushing its
        	// address into the argument list
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_PCTREF, 0, 1)) {
                scopectx_t scope = parser_scope_get(pctx);
                textpos_t pos = parser_curpos(pctx);
                char tnbuf[32];
                strdesc_t str;
                name_t *tmpsym;
                data_attr_t attr;
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
                    expr_signal(ctx, STC__DELIMEXP, "(");
                }
                if (!expr_parse_expr(ctx, &exp)) {
                    expr_signal(ctx, STC__EXPREXP);
                }
                if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
                    expr_signal(ctx, STC__DELIMEXP, ")");
                }
                if (!expr_has_value(exp)) {
                    expr_signal(ctx, STC__EXPRVALRQ);
                }
                strdesc_init(&str, tnbuf, 0);
                str.len = tempname_get(ctx->namectx, tnbuf, sizeof(tnbuf));
                memset(&attr, 0, sizeof(attr));
                attr.dclass = DCLASS_STACKONLY;
                attr.units = machine_scalar_units(ctx->mach);
                attr.flags = (machine_addr_signed(ctx->mach) ? SYM_M_SIGNEXT : 0);
                attr.ivlist = expr_initval_add(ctx, 0, exp, machine_scalar_units(ctx->mach));
                tmpsym = datasym_declare(scope, &str, SYMSCOPE_LOCAL, &attr, pos);
                arg = expr_node_alloc(ctx, EXPTYPE_PRIM_SEG, pos);
                expr_seg_name_set(arg, tmpsym);
            } else if (!expr_parse_expr(ctx, &arg)) {
            	// Use a zero literal expression for missing/erroneous parameters
                arg = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
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
                expr_signal(ctx, STC__DELIMEXP, ",");
                parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
                return 0;
            }

        } /* argument loop */
    }

    // XXX validate the arguments against the routine declaration, if we can

    exp = expr_node_alloc(ctx, EXPTYPE_PRIM_RTNCALL, parser_curpos(pctx));
    expr_rtnaddr_set(exp, rtn);
    exprseq_append(expr_rtn_inargs(exp), &inargs);
    exprseq_append(expr_rtn_outargs(exp), &outargs);

    // This expression has a value unless it was a NOVALUE routine
    if (expr_type(rtn) == EXPTYPE_PRIM_SEG) {
        name_t *np = expr_seg_name(rtn);
        if (name_type(np) == LEXTYPE_NAME_ROUTINE) {
            routine_attr_t *attr = rtnsym_attr(np);
            expr_has_value_set(exp, (attr->flags & SYM_M_NOVALUE) == 0);
            return exp;
        }
    }
    expr_has_value_set(exp, 1);
    return exp;

} /* parse_arglist */

/*
 * parse_fldref
 *
 * Looks for the opening angle bracket of a field reference, and
 * parses it if present.
 *
 * primary-expr OR executable-function-call <{args}>
 * Arguments, if present, are:
 *  P - starting bit position
 *	S - size (number of bits)
 *  E - sign-extension indicator (0=unsigned, 1=signed)
 *
 * Returns 1 on success, 0 otherwise.  If successful, a pointer to
 * the resulting FLDREF expression node is stored in expp.
 */
static int
parse_fldref (expr_ctx_t ctx, expr_node_t **expp) {

    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *pos, *size, *exp;
    long signext = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LANGLE, 0, 1)) {
        return 0;
    }
    pos = size = 0;
    if (!expr_parse_expr(ctx, &pos)) {
        expr_signal(ctx, STC__EXPREXP);
        pos = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ",");
    }
    if (!expr_parse_expr(ctx, &size)) {
        expr_signal(ctx, STC__EXPREXP);
        size = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
    }
	// Sign-extension must be a compile-time constant expression
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        if (!expr_parse_ctce(ctx, 0, &signext) || (signext != 0 && signext != 1)) {
            expr_signal(ctx, STC__EXPCTCE);
            signext = 0;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RANGLE, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ">");
    }

	// Handle degenerate cases:
	//	1. If the expression being field-referenced is a literal, do
	//     the field-extraction now.
	//  2. If the expression being field-referenced is a segment, and
	//     the position aligns to an addressable boundary, and
	//     the size is an even multiple of addressable-unit bits,
	// 	   stash the offset and number of AUs directly into the
	//	   segment expression.
	//  We can only do this if the position and size fields are CTCEs.
	//
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
        if (expr_type(*expp) == EXPTYPE_PRIM_SEG &&
            expr_litval(pos) % machine_unit_bits(mach) == 0 &&
            expr_litval(size) == machine_scalar_bits(mach)) {
            data_attr_t *attr = datasym_attr(expr_seg_name(*expp));
            if (attr->dclass != DCLASS_ARG && attr->dclass != DCLASS_REGISTER) {
                expr_seg_offset_set(*expp, expr_seg_offset(*expp) +
                                    expr_litval(pos) / machine_unit_bits(mach));
                expr_seg_units_set(*expp, machine_scalar_units(mach));
                expr_seg_signext_set(*expp, (signext != 0));
                expr_node_free(ctx, pos);
                expr_node_free(ctx, size);
                return 1;
            }
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

/*
 * parse_codecomment
 *
 * CODECOMMENT 'string'{,...}: block-expression
 *
 * Parses a codecomment, which must be followed by a block.
 * This results in a block expression node with the codecomment
 * annotation attached to it.
 */
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
        codecomment = string_append(ctx->strctx, codecomment, lexeme_text(lex));
        lexeme_free(ctx->lctx, lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            break;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ":");
    }
    while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
        namereflist_instail(&labels,
                       nameref_alloc(namectx, lexeme_ctx_get(lex)));
        lexeme_free(ctx->lctx, lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ":");
        }
    }
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt != LEXTYPE_DELIM_LPAR && lt != LEXTYPE_EXP_DELIM_BEGIN) {
        expr_signal(ctx, STC__BLOCKEXP);
        string_free(ctx->strctx, codecomment);
        return 0;
    }
    lexeme_free(ctx->lctx, lex);
    if (!parse_block(ctx, lt, &exp, codecomment, &labels)) {
        return 0;
    }
    return exp;

} /* parse_codecomment */

/*
 * parse_primary
 *
 * Parses a primary-expression that isn't automatically
 * handled through the expression binder: blocks and literals.
 * The expression binder handles segment names.
 */
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
        lexeme_free(ctx->lctx, lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ":");
        }
        while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
            namereflist_instail(&labels,
                                nameref_alloc(namectx, lexeme_ctx_get(lex)));
            lexeme_free(ctx->lctx, lex);
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                expr_signal(ctx, STC__DELIMEXP, ":");
            }
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
            if (!parse_block(ctx, lt, &exp, 0, &labels)) {
                return 0;
            }
        } else {
            expr_signal(ctx, STC__BLOCKEXP);
            return 0;
        }
    } else if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
        lexeme_free(ctx->lctx, lex);
        if (!parse_block(ctx, lt, &exp, 0, &labels)) {
            return 0;
        }
    } else if (lt == LEXTYPE_NUMERIC) {
        strdesc_t *ltext = lexeme_text(lex);
        long val;
        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
        if (!string_numval(ltext, 10, &val)) {
            expr_signal(ctx, STC__NUMCNVERR, ltext->ptr, ltext->len);
            val = 0;
        }
        expr_litval_set(exp, val);
        expr_is_ctce_set(exp, 1);
        expr_has_value_set(exp, 1);
    } else if (lt == LEXTYPE_STRING) {
        int i;
        strdesc_t *text = lexeme_text(lex);
        unsigned long val = 0;
        int len = text->len;
        if (len > machine_scalar_maxbytes(ctx->mach) && !ctx->longstringsok) {
            expr_signal(ctx, STC__STRLENERR);
            len = machine_scalar_maxbytes(ctx->mach);
        }
        exp = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(pctx));
        expr_litstring_set(exp, string_copy(ctx->strctx, 0, text));
        for (i = 0; i < len; i++) {
            val = val | (*(text->ptr+i) << (i*8));
        }
        expr_litval_set(exp, val);
        expr_is_ctce_set(exp, 1);
        expr_has_value_set(exp, 1);
    }

    if (exp == 0) {
        return exp;
    }

	// Any primary can be followed by (...) to become a routine-call.
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_node_t *rtncall = expr_parse_arglist(ctx, exp);
        if (rtncall == 0) {
            expr_signal(ctx, STC__SYNTAXERR);
            expr_node_free(ctx, exp);
            return 0;
        }
        exp = rtncall;
    }

    // XXX Must also check for general routine calls, once we
    //  have linkages worked in

    // At this point, 'exp' is the primary expression.
    // If it's not already a field-ref, see if we've got one.
    if (expr_type(exp) != EXPTYPE_PRIM_FLDREF) {
        parse_fldref(ctx, &exp);
    }

    return exp;

} /* parse_primary */

/*
 * find_base_segname
 *
 * Searches for the name representing a segment
 * being referenced in an expression (either a segment
 * primary or an operator expression that has a segment
 * primary as an operand).
 */
static name_t *
find_base_segname (expr_node_t *expr)
{
    name_t *np;

    if (expr == 0) {
        return 0;
    }
    if (expr_type(expr) == EXPTYPE_PRIM_SEG) {
        return expr_seg_name(expr);
    }
    if (expr_type(expr) != EXPTYPE_OPERATOR) {
        return 0;
    }
    np = find_base_segname(expr_op_lhs(expr));
    if (np == 0) {
        np = find_base_segname(expr_op_rhs(expr));
    }
    return np;

} /* find_base_segname */

/*
 * update_xtce_bits
 *
 * Updates the CTCE and LTCE flags in an operator
 * expression, based on the operator type and the
 * operands' CTCE/LTCE status.
 */
static void
update_xtce_bits (expr_node_t *opexpr)
{
    optype_t op = expr_op_type(opexpr);

	// Fetch and assign are never CTCE or LTCE, obviously
    if (op == OPER_FETCH || op == OPER_ASSIGN) {
        expr_is_ctce_set(opexpr, 0);
        expr_is_ltce_set(opexpr, 0);
        return;
    }
    // For unary operators, inherit from the right-hand side only
    // (since LHS is null)
    if (oper_is_unary(op)) {
        expr_is_ctce_set(opexpr, expr_is_ctce(expr_op_rhs(opexpr)));
        expr_is_ltce_set(opexpr, 0);
        return;
    }
    // For all other operators, the result is CTCE if both operands are CTCEs.
    if (expr_is_ctce(expr_op_lhs(opexpr)) && expr_is_ctce(expr_op_rhs(opexpr))) {
        expr_is_ctce_set(opexpr, 1);
        return;
    }
    // If the LHS is LTCE (checked using is_ltce_only here, since
    // normally CTCEs also qualify as LTCEs):
    //	- adding or subtracting a CTCE gives an LTCE result
    //  - address comparison or subtraction with another LTCE
    //		that is either static and resides in the same psect
    //      or is external and references the same external symbol
    //		gives an LTCE result.
    if (expr_is_ltce_only(expr_op_lhs(opexpr))) {
        if ((op == OPER_ADD || op == OPER_SUBTRACT) &&
            expr_is_ctce(expr_op_rhs(opexpr))) {
            expr_is_ltce_set(opexpr, 1);
            return;
        }
        if ((op == OPER_SUBTRACT ||
             (op >= OPER_CMP_EQLA && op <= OPER_CMP_GEQA)) &&
             expr_is_ltce_only(expr_op_rhs(opexpr))) {
            name_t *np_lhs, *np_rhs;
            np_lhs = find_base_segname(expr_op_lhs(opexpr));
            np_rhs = find_base_segname(expr_op_rhs(opexpr));
            if (np_lhs != 0 && np_rhs != 0) {
                expr_is_ltce_set(opexpr,
                                 sym_addrs_comparable(np_lhs, np_rhs));
            }
            return;
        }
    }
    // The LRM doesn't explicitly allow this, but we also
    // check for an LTCE on the right-hand side added to
    // to CTCE on the left-hand side.
    if (expr_is_ltce_only(expr_op_rhs(opexpr))) {
        if (op == OPER_ADD &&
            expr_is_ctce(expr_op_lhs(opexpr))) {
            expr_is_ltce_set(opexpr, 1);
            return;
        }
    }
} /* update_xtce_bits */

/*
 * parse_operand
 *
 * Parse an expression that could be an operand
 * in an operator expression.  That is, it could
 * be a primary or an executable function call.
 */
static int
parse_operand (expr_ctx_t ctx, expr_node_t **expp)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_dispatch_fn dfunc;
    expr_node_t *exp;
    lexeme_t *lex;
    optype_t op;
    lextype_t lt;

    exp = 0;
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (check_unary_op(lt, &op)) {
        lexeme_free(ctx->lctx, lex);
        if (!parse_operand(ctx, &exp)) {
            expr_signal(ctx, STC__SYNTAXERR);
            return 0;
        }
        *expp = parse_op_expr(ctx, op, 0, exp);
        return 1;
    }
    dfunc = lookup_dispatcher(ctx, lt);
    if (dfunc != 0) {
        exp = dfunc(ctx, lt, lex);
        if (exp != 0) {
            lexeme_free(ctx->lctx, lex);

        }
    }
    if (exp == 0) {
        exp = parse_primary(ctx, lt, lex);
        if (exp == 0) {
            parser_insert(pctx, lex);
            *expp = 0;
            return 0;
        }
    }

    if (expr_is_primary(exp) &&
        expr_type(exp) != EXPTYPE_PRIM_FLDREF) {
        parse_fldref(ctx, &exp);
    }

    *expp = exp;
    return 1;

} /* parse_operand */

/*
 * parse_op_expr
 *
 * Parse an operator expression, given the operator and its operand(s) -
 * a single RHS operand for unary operators, both LHS and RHS for other
 * operators.  Note that the operand(s) could be operator expressions
 * which need to be merged appropriately with the current operator based
 * on operator precedence and associativity.  That is handled here.
 *
 * Control expressions are never permitted to be operands.
 */
static expr_node_t *
parse_op_expr (expr_ctx_t ctx, optype_t curop, expr_node_t *lhs, expr_node_t *rhs)
{
    expr_node_t *thisnode;
    optype_t op;
    int normal;

    if (expr_is_noop(lhs)) {
        if (!oper_is_unary(curop)) {
            expr_signal(ctx, STC__INTCMPERR, "parse_op_expr[1]");
            expr_node_free(ctx, lhs);
            lhs = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(ctx->pctx));
            expr_has_value_set(lhs, 1);
        }
    } else {
        if (expr_is_ctrl(lhs)) {
            expr_signal(ctx, STC__EXPRVALRQ);
            // but try anyway
            expr_node_free(ctx, lhs);
            lhs = expr_node_alloc(ctx, EXPTYPE_PRIM_LIT, parser_curpos(ctx->pctx));
            expr_has_value_set(lhs, 1);
        }
    }

    if (expr_is_noop(rhs) || expr_is_ctrl(rhs)) {
        expr_signal(ctx, STC__EXPRVALRQ);
    }

    // OK, the operands are valid.  If they are operator expressions,
    // we need to lok at the precedence and associativity.

    // It is invalid to have a unary operator on our right
    // that has lower priority.
    if (expr_is_opexp(rhs) && oper_is_unary(expr_op_type(rhs))) {
        if (op_priority(expr_op_type(rhs)) < op_priority(curop)) {
            expr_signal(ctx, STC__SYNTAXERR);
        }
    }

    normal = 1;

    // If the LHS operator expression has higher precedence, or
    // equal precedence but right-to-left associativity, it should
    // just stay as the current operator's LHS.  Hence, 'normal'.
    // Otherwise, we need to make this node the RHS of the
    // LHS's operator, and put the RHS of the LHS's operator in as
    // our LHS operand.

    if (expr_is_opexp(lhs)) {
        normal = 0;
        op = expr_op_type(lhs);
        if (op_priority(op) > op_priority(curop) ||
            (op_priority(op) == op_priority(curop) && op_is_r2l(curop))) {
            normal = 1;
        }
    }

    thisnode = expr_node_alloc(ctx, EXPTYPE_OPERATOR, expr_textpos(rhs));
    if (thisnode == 0) {
        expr_signal(ctx, STC__OUTOFMEM, "parse_op_expr[4]");
        return 0;
    }
    expr_op_type_set(thisnode, curop);
    expr_op_rhs_set(thisnode, rhs);
    if (normal) {
        expr_op_lhs_set(thisnode, lhs);
        expr_has_value_set(thisnode, 1);
        update_xtce_bits(thisnode);
        return thisnode;
    } else {
        expr_op_lhs_set(thisnode, expr_op_rhs(lhs));
        expr_op_rhs_set(lhs, thisnode);
        expr_has_value_set(thisnode, 1);
        update_xtce_bits(thisnode);
        update_xtce_bits(lhs);
        return lhs;
    }

} /* parse_op_expr */

/*
 * reduce_op_expr
 *
 * This rather lengthy routine recursively descends
 * into an operator expression's node tree, looking for
 * simple reductions:
 *
 *   CTCE <op> CTCE => value of resulting operation
 *   <anything> {add/subtract} <literal zero> => <anything>
 *   <address> {add/subtract} CTCE => adjust offset in the segment expression
 *   <anything> {times} <literal one> => <anything>
 *
 * Be careful about adding any further reductions/improvements
 * here.  For example, <anything> {times} <literal zero> results
 * in zero, but <anything> could have side effects that we cannot
 * legitimately ignore here.
 */
void
reduce_op_expr (expr_ctx_t ctx, expr_node_t **nodep) {
    expr_node_t *node, *lhs, *rhs;

    if (nodep == 0) {
        return;
    }

    node = *nodep;

    if (!expr_is_opexp(node)) {
        return;
    }

    lhs = expr_op_lhs(node);
    if (expr_is_opexp(lhs)) {
        reduce_op_expr(ctx, &lhs);
        expr_op_rhs_set(node, lhs);
    }
    rhs = expr_op_rhs(node);
    if (expr_is_opexp(rhs)) {
        reduce_op_expr(ctx, &rhs);
        expr_op_rhs_set(node, rhs);
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
                expr_litstring_set(node, 0);
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
                        expr_signal(ctx, STC__DIVBYZERO);
                        result = 0;
                    } else {
                        result = leftval / rightval;
                    }
                    break;
                case OPER_MODULO:
                    if (rightval == 0) {
                        expr_signal(ctx, STC__DIVBYZERO);
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
                data_attr_t *attr = datasym_attr(expr_seg_name(rhs));
                if (attr->dclass != DCLASS_REGISTER && attr->dclass != DCLASS_ARG) {
                    expr_node_t *tmp = rhs;
                    expr_op_rhs_set(node, 0);
                    expr_seg_name_set(tmp, expr_seg_name(rhs));
                    expr_seg_offset_set(tmp, expr_seg_offset(tmp)+expr_litval(lhs));
                    expr_node_free(ctx, node);
                    *nodep = tmp;
                    return;
                }
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
                data_attr_t *attr = datasym_attr(expr_seg_name(lhs));
                if (attr->dclass != DCLASS_REGISTER && attr->dclass != DCLASS_ARG) {
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
 * expr_parse_expr
 *
 * The main entry point for parsing an expression of any
 * kind.
 */
int
expr_parse_expr (expr_ctx_t ctx, expr_node_t **expp)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *lhs, *rhs;
    lexeme_t *lex;
    lextype_t lt;
    optype_t op;
    int status = 0;

    lhs = rhs = 0;
	// Loop to keep building an operator expression
	// as long as we have valid operands and an operator
	// to process.
    while (1) {
        if (lhs == 0) {
            if (!parse_operand(ctx, &lhs)) {
                break;
            }
        }
        if (lhs == 0) {
            break;
        }
        if (expr_is_primary(lhs) || expr_is_execfun(lhs) ||
            expr_is_opexp(lhs)) {
            lt = parser_next(pctx, QL_NORMAL, &lex);
            op = lextype_to_optype(lt);
            if (op == OPER_NONE) {
                parser_insert(pctx, lex);
                break;
            }
            lexeme_free(ctx->lctx, lex);
            if (!parse_operand(ctx, &rhs)) {
                expr_signal(ctx, STC__SYNTAXERR);
                break;
            }

            lhs = parse_op_expr(ctx, op, lhs, rhs);
            if (lhs == 0) {
                break;
            }
        } else {
            break;
        }
    }

    if (lhs != 0 && expr_type(lhs) == EXPTYPE_OPERATOR) {
        reduce_op_expr(ctx, &lhs);
    }

    if (lhs != 0) {
        *expp = lhs;
        status = 1;
    }

    return status;

} /* parse_expr */

/*
 * expr_parse_seq
 *
 * Parse an expression from a given lexeme sequence.  The
 * sequence is expected to contain exactly one complete
 * expression.
 *
 * Returns 1 if successful, and sets expp to point to the
 * resulting expression.  Returns 0 on failure.  In both
 * cases, the entire lexeme sequence is consumed by this
 * routine.
 */
int
expr_parse_seq (expr_ctx_t ctx, lexseq_t *seq, expr_node_t **expp)
{
    parse_ctx_t pctx = ctx->pctx;
    int status;

    parser_insert(pctx, lexeme_create(ctx->lctx, LEXTYPE_MARKER, &nullstr));
    parser_insert_seq(pctx, seq);
    *expp = 0;
    status = expr_parse_expr(ctx, expp);
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

} /* expr_parse_seq */

/*
 * expr_parse_ctce
 *
 * Parses a compile-time constant expression.
 * Returns 0 on failure, 1 (and sets lexp and valp
 * if they are non-NULL) on success.
 */
int
expr_parse_ctce (expr_ctx_t ctx, lexeme_t **lexp, long *valp)
{
    parse_ctx_t pctx = ctx->pctx;
    expr_node_t *exp = 0;
    long val;

    ctx->longstringsok = 1;
    if (!expr_parse_expr(ctx, &exp)) {
        ctx->longstringsok = 0;
        return 0;
    }
    ctx->longstringsok = 0;

    if (expr_type(exp) != EXPTYPE_PRIM_LIT) {
        expr_node_free(ctx, exp);
        return 0;
    }
    val = expr_litval(exp);
    if (lexp != 0) {
        strdesc_t *str = string_printf(ctx->strctx, 0, "%ld", expr_litval(exp));
        *lexp = parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str);
        (*lexp)->type = LEXTYPE_NUMERIC;
        string_free(ctx->strctx, str);
    }
    if (valp != 0) {
        *valp = val;
    }
    expr_node_free(ctx, exp);
    return 1;

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

    if (!expr_parse_expr(ctx, &exp) || exp == 0) {
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
        expr_signal(ctx, STC__BLOCKEXP);
        return 0;
    }

    return parse_block(ctx, lt, blockexp, 0, &labels);

} /* expr_parse_block */


/*
 * expr_initval_add
 *
 * Convenience function for adding an initializer based on
 * an expression.  Literal expressions are converted to literal
 * initializers.
 */
initval_t *
expr_initval_add (expr_ctx_t ctx, initval_t *ivlist, expr_node_t *exp,
                  unsigned int width)
{
    long val;

    switch (expr_type(exp)) {
        case EXPTYPE_PRIM_LIT:
            val = expr_litval(exp);
            expr_node_free(ctx, exp);
            return initval_scalar_add(ctx->symctx, ivlist, 1, val, width, 0);
        default:
            return initval_expr_add(ctx->symctx, ivlist, 1, exp, width, 0);
            break;
    }

} /* expr_initval_add */

/*
 * parse_xCTE
 *
 * %CTCE(exp,...)
 * %LTCE(exp,...)
 *
 * Inserts a literal 1 if all of the arguments are constant
 * expressions (compile-time if checkltce is zero, link-time
 * if 1); otherwise, inserts a literal 0.
 */
static int
parse_xTCE (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt)
{
    expr_ctx_t ctx = vctx;
    expr_node_t *exp;
    int allok = 1;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 0;
    }
    ctx->longstringsok = 1;
    while (1) {
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            allok = 0;
            break;
        }
        if (lt == LEXTYPE_LXF_CTCE) {
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
            expr_signal(ctx, STC__DELIMEXP, ",");
        }
    }
    ctx->longstringsok = 0;
    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, (allok ? &one : &zero)));
    return 1;

} /* parse_xCTE */

/*
 * %EXACTSTRING(n, fill, #p...)
 *
 * Returns a string that is exactly 'n' characters,
 * either truncated, or filled with the 'fill' character.
 * If only two parameters given, return null string.
 */
static int
parse_EXACTSTRING (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    lexeme_t *lex;
    char fillchr = 0;
    size_t len = 0;
    long val;
    strdesc_t *str;
    int i;
    static lextype_t terms[2] = { LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_RPAR };

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }
    if (!expr_parse_ctce(ctx, 0, &val)) {
        expr_signal(ctx, STC__EXPCTCE);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    len = val;
    if (len > STRING_MAXLEN) {
        expr_signal(ctx, STC__INVSTRLIT);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 0)) {
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (!expr_parse_ctce(ctx, 0, &val)) {
        expr_signal(ctx, STC__EXPCTCE);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    fillchr = val & 0xff;
    if (val > 255) {
        strdesc_t *valstr = string_printf(ctx->strctx, 0, "%ld", val);
        expr_signal(ctx, STC__INVCHRVAL, valstr);
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        string_free(ctx->strctx, valstr);
        return 1;
    }

    i = parser_expect_oneof(pctx, QL_NORMAL, terms, 2, 0, 1);
    if (i < 0) {
        expr_signal(ctx, STC__DELIMEXP, ",");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        return 1;
    }
    if (i == 1) { // the right paren
        parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_STRING, &nullstr));
        return 1;
    }

    lex = parse_string_params(pctx, 1);
    if (lexeme_boundtype(lex) != LEXTYPE_STRING) {
        expr_signal(ctx, STC__INVSTRLIT);
        lexeme_free(expr_lexmemctx(ctx), lex);
        return 1;
    }
    str = string_copy(ctx->strctx, 0, lexeme_text(lex));
    if (str->len < len) {
        strdesc_t *filldsc;
        filldsc = string_alloc(ctx->strctx, 0, len - str->len);
        if (filldsc == 0) {
            expr_signal(ctx, STC__OUTOFMEM);
        } else {
            memset(filldsc->ptr, fillchr, filldsc->len);
            str = string_append(ctx->strctx, str, filldsc);
            if (str == 0) {
                expr_signal(ctx, STC__OUTOFMEM);
            }
            string_free(ctx->strctx, filldsc);
        }
    } else if (str->len > len) {
        str->len = len;
    }

    lexeme_free(expr_lexmemctx(ctx), lex);
    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_STRING, str));
    string_free(ctx->strctx, str);

    return 1;

} /* parse_EXACTSTRING */

/*
 * %CHAR(code,...)
 *
 * Return a quoted string that consists of the specified
 * characters.  Each 'code' must be a compile-time constant
 * expression in the range 0-255.
 */
static int
parse_CHAR (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    int skip_to_paren = 0;
    int hit_error = 0;
    strdesc_t chdsc;
    strdesc_t *result;
    char ch;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 0)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }

    result = string_alloc(ctx->strctx, 0, 0);
    strdesc_init(&chdsc, &ch, 1);
    while (1) {
        if (!expr_parse_ctce(ctx, 0, &val)) {
            expr_signal(ctx, STC__EXPCTCE);
            hit_error = 1;
            skip_to_paren = 1;
            break;
        }
        if (val > 255) {
            strdesc_t *text = string_printf(ctx->strctx, 0, "%ld", val);
            expr_signal(ctx, STC__NUMCNVERR, text->ptr, text->len);
            string_free(ctx->strctx, text);
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
        ch = val & 0xff;
        result = string_append(ctx->strctx, result, &chdsc);
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
            skip_to_paren = 1;
            hit_error = 1;
            break;
        }
    }

    if (hit_error) {
        if (skip_to_paren) {
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        }
    } else {
        parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_STRING, result));
    }
    string_free(ctx->strctx, result);

    return 1;

} /* parse_CHAR */


/*
 * parse_ISSTRING
 *
 * %ISSTRING(exp,...)
 *
 * Returns 1 if every expression results in a quoted-string
 * lexeme, otherwise 0.  Note that this up-calls to the expression
 * module, since the parameters are expressions.
 */
static int
parse_ISSTRING (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t lt)
{
    expr_ctx_t ctx = vctx;
    expr_node_t *exp;
    int allstr = 1;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 0;
    }
    ctx->longstringsok = 1;
    while (1) {
        if (!expr_parse_expr(ctx, &exp)) {
            expr_signal(ctx, STC__EXPREXP);
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
            expr_signal(ctx, STC__DELIMEXP, ",");
        }
    }

    ctx->longstringsok = 0;
    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                                             (allstr ? &one : &zero)));
    return 1;

} /* parse_ISSTRING */

/*
 * %IF lexical-test %THEN ... [ %ELSE ... ] %FI
 *
 * Lexical conditional processing.
 *  - macros using %IF must have a fully formed sequence
 *  - end-of-file not permitted in the middle of this sequence
 *  - must handle nested sequences!!
 *  - The test is TRUE only if the *** low-order bit *** is 1
 *
 * We use a stack of state variables to track our current lexical-conditional
 * state.  State values are:
 *
 * COND_NORMAL - not in a lexical conditional
 * COND_CWA    - in a consequent (%THEN sequence), but want alternative
 * COND_CWC    - in a consequent, and want the consequent
 * COND_AWA    - in an alternative (%ELSE sequence), and want it
 * COND_AWC    - in an alternative, but want the consequent
 *
 * parser_next() ignores lexemes other than %ELSE and %FI while our
 * current state is CWA or AWC.
 *
 * If we encounter a new %IF while in a state other than COND_NORMAL,
 * the current state is stacked and we move to a new condlevel.
 *
 * NB: %THEN, %ELSE, and %FI are handled in the parser module.  %IF
 *     is handled here because we need to parse an expression.
 */
static int
parse_IF (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    long testval;

    // Note the use of increment/decrement here -- this is to handle
    // nesting of instances in which hitting end-of-file is a no-no.
    parser_incr_erroneof(pctx);

    if (!expr_parse_ctce(ctx, 0, &testval)) {
        expr_signal(ctx, STC__EXPCTCE);
        parser_decr_erroneof(pctx);
        return 1;
    }

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_LXF_THEN, 0, 1)) {
        expr_signal(ctx, STC__KWDEXP, "%THEN");
        parser_decr_erroneof(pctx);
        return 1;
    }

    if (!parser_condstate_push(pctx, ((testval & 1) ? COND_CWC : COND_CWA))) {
        parser_decr_erroneof(pctx);
    }

    return 1;
    
} /* parse_IF */

/*
 * %NUMBER(n)
 *
 * Return a numeric lexeme representing the value
 * of 'n', which can be a (numeric) string literal,
 * a numeric literal, or the name of a COMPILETIME
 * or LITERAL.
 */
static int
parse_NUMBER (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    lexctx_t lctx = expr_lexmemctx(ctx);
    lextype_t lt;
    lexeme_t *lex, *rlex;
    long val;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }
    rlex = 0;
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt == LEXTYPE_STRING) {
        if (!string_numval(lexeme_text(lex), 10, &val)) {
            strdesc_t *text = lexeme_text(lex);
            expr_signal(ctx, STC__NUMCNVERR,
                       text->ptr, text->len);
            lexeme_free(lctx, lex);
            rlex = lexeme_create(lctx, LEXTYPE_STRING, &zero);
        } else {
            rlex = lex;
        }
    } else {
        strdesc_t *str;
        parser_insert(pctx, lex);
        if (!expr_parse_ctce(ctx, 0, &val)) {
            val = 0;
            expr_signal(ctx, STC__EXPCTCE);
        }
        str = string_printf(ctx->strctx, 0, "%ld", val);
        rlex = lexeme_create(lctx, LEXTYPE_STRING, str);
        string_free(ctx->strctx, str);
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
        parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
        if (rlex != 0) {
            lexeme_free(lctx, rlex);
        }
        return 1;
    }

    parser_insert(pctx, rlex);

    return 1;

} /* parse_NUMBER */

/*
 * %NBITS(n,...)
 * %NBITSU(n,..)
 *
 * Returns the largest number of bits required to hold the
 * value of the (compile-time-constant) expressions.  In the U-form,
 * the expressions are treated as unsigned values.
 */
static int
parse_nbits_func (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    strdesc_t *bcstr;
    long val;
    long maxbits = 0, thesebits;
    machinedef_t *mach = expr_machinedef(ctx);

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
        return 1;
    }
    while (1) {
        if (!expr_parse_ctce(ctx, 0, &val)) {
            expr_signal(ctx, STC__EXPCTCE);
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return 1;
        }
        if (curlt == LEXTYPE_LXF_NBITS) {
            thesebits = bits_needed((unsigned long) labs(val));
            if (val < 0 && thesebits < machine_scalar_bits(mach)-1) {
                thesebits = machine_scalar_bits(mach)-1;
            }
        } else {
            thesebits = bits_needed((unsigned long) val);
        }
        if (thesebits > maxbits) {
            maxbits = thesebits;
        }
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
            expr_signal(ctx, STC__DELIMEXP, ",");
            parser_skip_to_delim(pctx, LEXTYPE_DELIM_RPAR);
            return 1;
        }
    }
    if (maxbits > machine_scalar_bits(mach) - (curlt == LEXTYPE_LXF_NBITS ? 1 : 0)) {
        expr_signal(ctx, STC__NBITSERR, maxbits, machine_scalar_bits(mach));
        maxbits = machine_scalar_bits(mach);
    }
    bcstr = string_printf(ctx->strctx, 0, "%ld", maxbits);
    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC, bcstr));
    string_free(ctx->strctx, bcstr);
    return 1;

} /* parse_nbits_func */

/*
 * parse_ALLOCATION
 *
 * %ALLOCATION(name)
 *   Returns a numeric literal representing the number of addressable
 *   units allocated to <name> (which must have allocated storage).
 */
static int
parse_ALLOCATION (parse_ctx_t pctx, void *vctx, quotelevel_t ql, lextype_t curlt)
{
    expr_ctx_t ctx = vctx;
    lexeme_t *lex;
    name_t *np;
    data_attr_t attr;

    attr.units = 0;
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, "(");
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_DATA, &lex, 1)) {
        expr_signal(ctx, STC__SEGNAMEXP);
    } else {
        np = datasym_search(parser_scope_get(pctx), lexeme_text(lex), &attr);
        if (np == 0) {
            expr_signal(ctx, STC__INTCMPERR); // should never happen
            attr.units = 0;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        expr_signal(ctx, STC__DELIMEXP, ")");
    }

    parser_insert(pctx, parser_lexeme_create(pctx, LEXTYPE_NUMERIC,
                                             string_printf(ctx->strctx, 0,
                                                           "%lu", attr.units)));
    return 1;

} /* parse_ALLOCATION */