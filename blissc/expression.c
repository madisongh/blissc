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
#include "storage.h"
#include "parser.h"
#include "nametable.h"
#include "lexeme.h"
#include "machinedef.h"

#undef DOEXPTYPE
#define DOEXPTYPE(t_) "EXPTYPE_" #t_,
static const char *exptypenames[] = { DOEXPTYPES };
#undef DOEXPTYPE

#define ALLOC_QTY       128
static expr_node_t *freenodes = 0;

static name_t expr_names[] = {
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
    NAMEDEF("DO", LEXTYPE_CTRL_DO, NAME_M_RESERVED)
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
    OPMAP(EQLA,COUNT) OPMAP(NEQA,COUNT) \
    OPMAP(LSSA,COUNT) OPMAP(LEQA,COUNT) \
    OPMAP(GTRA,COUNT) OPMAP(GEQA,COUNT)
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
    { 20, 3 }, // unary NOT
    { 15, 0 }, { 10, 0 }, { 5, 0 }, { 5, 0 }, // AND, OR, EQV, XOR
    { 0, 1 } // assignment
};

#define DOOPTYPE(t_) "OPER_" #t_,
static const char *oper_names[] = { DOOPTYPES };
#undef DOOPTYPE

static int parse_expr(parse_ctx_t pctx, expr_node_t **expp,
                      int recursed, int lstrok);
static int parse_condexp(void *pctx, quotelevel_t ql,
                         quotemodifier_t qm, lextype_t lt, condstate_t cs,
                         lexeme_t *orig, lexseq_t *result);
static int parse_case(void *pctx, quotelevel_t ql,
                         quotemodifier_t qm, lextype_t lt, condstate_t cs,
                         lexeme_t *orig, lexseq_t *result);
static int parse_select(void *pctx, quotelevel_t ql,
                         quotemodifier_t qm, lextype_t lt, condstate_t cs,
                         lexeme_t *orig, lexseq_t *result);
static int parse_incrdecr(void *pctx, quotelevel_t ql,
                          quotemodifier_t qm, lextype_t lt, condstate_t cs,
                          lexeme_t *orig, lexseq_t *result);
static int get_ctce(parse_ctx_t pctx, stgctx_t stg, long *valp);

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
expr_node_alloc (exprtype_t type, textpos_t pos)
{
    expr_node_t *node;
    int i;

    if (freenodes == 0) {
        freenodes = malloc(ALLOC_QTY * sizeof(expr_node_t));
        if (freenodes == 0) {
            return 0;
        }
        for (i = 0, node = freenodes; i < ALLOC_QTY-1; i++, node++) {
            expr_next_set(node, node+1);
        }
        expr_next_set(node, 0);
    }
    node = freenodes;
    freenodes = expr_next(node);
    memset(node, 0, sizeof(expr_node_t));
    expr_type_set(node, type);
    expr_textpos_set(node, pos);
    return node;

} /* expr_node_alloc */

void
expr_node_free (expr_node_t *node, stgctx_t stg) {

    if (node == 0) {
        return;
    }
    switch (expr_type(node)) {
        case EXPTYPE_PRIM_SEG:
            seg_free(stg, expr_segval(node));
            break;
        case EXPTYPE_OPERATOR:
            expr_node_free(expr_op_lhs(node), stg);
            expr_node_free(expr_op_rhs(node), stg);
            break;
        case EXPTYPE_PRIM_BLK: {
            expr_node_t *snode;
            for (snode = expr_blk_seq(node); snode != 0; snode = expr_blk_seq(node)) {
                expr_blk_seq_set(node, expr_next(node));
                expr_node_free(snode, stg);
            }
            scope_end(expr_blk_scope(node));
            block_free(stg, expr_blk_block(node));
            break;
        }
        case EXPTYPE_PRIM_FLDREF:
            expr_node_free(expr_fldref_addr(node), stg);
            expr_node_free(expr_fldref_pos(node), stg);
            expr_node_free(expr_fldref_size(node), stg);
            break;
        case EXPTYPE_CTRL_COND:
            expr_node_free(expr_cond_test(node), stg);
            expr_node_free(expr_cond_consequent(node), stg);
            expr_node_free(expr_cond_alternative(node), stg);
            break;
        case EXPTYPE_CTRL_LOOPWU:
            expr_node_free(expr_wuloop_test(node), stg);
            expr_node_free(expr_wuloop_body(node), stg);
            break;
        case EXPTYPE_CTRL_LOOPID:
            expr_node_free(expr_idloop_init(node), stg);
            expr_node_free(expr_idloop_term(node), stg);
            expr_node_free(expr_idloop_step(node), stg);
            expr_node_free(expr_idloop_body(node), stg);
            break;
        case EXPTYPE_PRIM_RTNCALL: {
            expr_node_t *arg, *anext;
            expr_node_free(expr_rtnaddr(node), stg);
            if (expr_inargs(node, &arg) > 0) {
                while (arg != 0) {
                    anext = expr_next(arg);
                    expr_node_free(arg, stg);
                    arg = anext;
                }
            }
            if (expr_outargs(node, &arg) > 0) {
                while (arg != 0) {
                    anext = expr_next(arg);
                    expr_node_free(arg, stg);
                    arg = anext;
                }
            }
        }

        case EXPTYPE_CTRL_CASE: {
            expr_node_t **actarray = expr_case_cases(node);
            long which, lo, hi;
            expr_node_free(expr_case_index(node), stg);
            expr_node_free(expr_case_outrange(node), stg);
            lo = expr_case_lowbound(node);
            hi = expr_case_highbound(node);
            for (which = lo; which <= hi; which++) {
                expr_node_free(actarray[which], stg);
            }
            free(actarray);
            break;
        }
        case EXPTYPE_SELECTOR:
            expr_node_free(expr_selector_action(node), stg);
            expr_node_free(expr_selector_low(node), stg);
            expr_node_free(expr_selector_high(node), stg);
            break;
        case EXPTYPE_CTRL_SELECT: {
            expr_node_t *sel, *selnext, *subnext;
            expr_node_free(expr_sel_index(node), stg);
            for (sel = expr_sel_selectors(node); sel != 0; sel = selnext) {
                selnext = expr_next(sel);
                do {
                    subnext = expr_selector_next(sel);
                    expr_node_free(expr_selector_action(sel), stg);
                    expr_node_free(expr_selector_low(sel), stg);
                    expr_node_free(expr_selector_high(sel), stg);
                    expr_node_free(sel, stg);
                    sel = subnext;
                } while (sel != 0);
            }
            break;
        }

        case EXPTYPE_PRIM_LIT:
        case EXPTYPE_NOOP:
        case EXPTYPE_EXECFUN: // TBD XXX
            break;
    }
    memset(node, 0x77, sizeof(expr_node_t));
    expr_next_set(node, freenodes);
    freenodes = node;

} /* expr_node_free */

static optype_t
lextype_to_optype (lextype_t lt, int addr_signed)
{
    if (lt < LEXTYPE_OP_MIN || lt > LEXTYPE_OP_MAX) {
        return OPER_NONE;
    }
    // XXX This routine assumes a particular ordering of lextypes!
    if (lt >= LEXTYPE_OP_EQLA && lt <= LEXTYPE_OP_GEQA) {
        lt = lt - (addr_signed ? 12 : 6);
    }
    return opmap[lt-LEXTYPE_OP_MIN];
}

static int
parse_plit (void *pctx, quotelevel_t ql, quotemodifier_t qm,
            lextype_t curlt, condstate_t cs, lexeme_t *orig, lexseq_t *result)
{
    stgctx_t stg = parser_get_cctx(pctx);
    seg_t *pseg;

    if (cs == COND_CWA || cs == COND_AWC) {
        lexeme_free(orig);
        return 1;
    }
    if (qm == QM_QUOTE || ql != QL_NORMAL) {
        return 0;
    }

    pseg = define_plit(pctx, stg, curlt);
    if (pseg == 0) {
        /* XXX error condition */
        return -1;
    }
    orig->type = orig->boundtype = LEXTYPE_SEGMENT;
    lexeme_ctx_set(orig, pseg);
    return 0;
    
} /* parse_plit */

void
expr_init (scopectx_t kwdscope)
{
    int i;

    for (i = 0; i < sizeof(expr_names)/sizeof(expr_names[0]); i++) {
        name_insert(kwdscope, &expr_names[i]);
    }
    lextype_register(LEXTYPE_KWD_PLIT, parse_plit);
    lextype_register(LEXTYPE_KWD_UPLIT, parse_plit);
    lextype_register(LEXTYPE_CTRL_IF, parse_condexp);
    lextype_register(LEXTYPE_CTRL_CASE, parse_case);
    lextype_register(LEXTYPE_CTRL_SELECT, parse_select);
    lextype_register(LEXTYPE_CTRL_SELECTA, parse_select);
    lextype_register(LEXTYPE_CTRL_SELECTU, parse_select);
    lextype_register(LEXTYPE_CTRL_SELECTONE, parse_select);
    lextype_register(LEXTYPE_CTRL_SELECTONEA, parse_select);
    lextype_register(LEXTYPE_CTRL_SELECTONEU, parse_select);
    lextype_register(LEXTYPE_CTRL_INCR, parse_incrdecr);
    lextype_register(LEXTYPE_CTRL_INCRA, parse_incrdecr);
    lextype_register(LEXTYPE_CTRL_INCRU, parse_incrdecr);
    lextype_register(LEXTYPE_CTRL_DECR, parse_incrdecr);
    lextype_register(LEXTYPE_CTRL_DECRA, parse_incrdecr);
    lextype_register(LEXTYPE_CTRL_DECRU, parse_incrdecr);
}

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
parse_block (parse_ctx_t pctx, lextype_t curlt, expr_node_t **expp,
             strdesc_t *codecomment, lexseq_t *labels) {

    lextype_t lt;
    lexeme_t *lex;
    stgctx_t stg = parser_get_cctx(pctx);
    scopectx_t scope = 0;
    expr_node_t *exp = 0;
    expr_node_t *seq, *last;
    block_t *blk;
    int expr_count;
    textpos_t endpos;
    lextype_t closer = (curlt == LEXTYPE_EXP_DELIM_BEGIN ?
                        LEXTYPE_EXP_DELIM_END : LEXTYPE_DELIM_RPAR);


    lt = parser_next(pctx, QL_NORMAL, &lex);

    expr_count = 0;
    seq = last = 0;
    while (1) {
        if (lt == closer) {
            endpos = lexeme_textpos_get(lex);
            lexeme_free(lex);
            break;
        } else if (lt >= LEXTYPE_DCL_MIN && lt <= LEXTYPE_DCL_MAX) {
            lexeme_free(lex);
            if (expr_count != 0) {
                /* XXX error condition */
                parser_skip_to_delim(pctx, closer);
                endpos = parser_curpos(pctx);
                break;
            }
            if (scope == 0) {
                scope = parser_scope_begin(pctx);
                blk = block_alloc(stg, parser_curpos(pctx));
            }
            parse_declaration(pctx, lt);
            lt = parser_next(pctx, QL_NORMAL, &lex);
            continue;
        } else {
            parser_insert(pctx, lex);
        }
        exp = 0;
        if (!parse_expr(pctx, &exp, 0, 0)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, closer);
            endpos = parser_curpos(pctx);
            break;
        }
        expr_count += 1;
        if (seq == 0) {
            last = seq = exp;
        } else {
            expr_next_set(last, exp);
            last = exp;
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_SEMI) {
            // discard the expression's value
            lexeme_free(lex);
            lt = parser_next(pctx, QL_NORMAL, &lex);
        }
    }

    if (scope == 0 && codecomment == 0 && lexseq_empty(labels) &&
        expr_count <= 1) {
        *expp = (expr_count == 0 ?
                 expr_node_alloc(EXPTYPE_NOOP, endpos) :
                 seq);
        return 1;
    }
    if (scope != 0) {
        parser_scope_end(pctx);
    }
    exp = expr_node_alloc(EXPTYPE_PRIM_BLK, endpos);
    expr_blk_scope_set(exp, scope);
    expr_blk_block_set(exp, blk);
    expr_blk_seq_set(exp, seq);
    expr_blk_codecomment_set(exp, codecomment);
    *expp = exp;
    // Now point the labels at us
    while ((lex = lexseq_remhead(labels))) {
        name_t *np = lexeme_ctx_get(lex);
        if (name_data(np) != 0) {
            /* XXX error condition */
        }
        *(expr_node_t **)name_data(np) = exp;
        lexeme_free(lex);
    }

    return 1;

} /* parse_block */

static int
parse_wu_loop (parse_ctx_t pctx, lextype_t opener, expr_node_t **expp)
{
    expr_node_t *body, *test, *exp;

    if (!parse_expr(pctx, &test, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!parse_expr(pctx, &body, 0, 0)) {
        /* XXX error condition */
        return 0;
    }

    exp = expr_node_alloc(EXPTYPE_CTRL_LOOPWU, parser_curpos(pctx));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, LOOP_PRETEST);
    expr_wuloop_body_set(exp, body);
    *expp = exp;

    return 1;

} /* parse_wu_loop */

static int
parse_do_loop (parse_ctx_t pctx, lextype_t opener, expr_node_t **expp)
{
    lextype_t lt;
    lexeme_t *lex;
    expr_node_t *body, *test, *exp;

    if (!parse_expr(pctx, &body, 0, 0)) {
        /* XXX error condition */
        return 0;
    }
    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt != LEXTYPE_CTRL_WHILE && lt != LEXTYPE_CTRL_UNTIL) {
        /* XXX error condition */
        parser_insert(pctx, lex);
        return 0;
    }
    lexeme_free(lex);

    if (!parse_expr(pctx, &test, 0, 0)) {
        /* XXX error condition */
        return 0;
    }

    exp = expr_node_alloc(EXPTYPE_CTRL_LOOPWU, parser_curpos(pctx));
    expr_wuloop_test_set(exp, test);
    expr_wuloop_type_set(exp, LOOP_POSTTEST);
    expr_wuloop_body_set(exp, body);
    *expp = exp;
    return 1;

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
static int
parse_arglist (parse_ctx_t pctx, expr_node_t *rtn, expr_node_t **expp)
{
    int incount, outcount, doing_outargs;
    expr_node_t *arg, *lastarg, *inargs, *outargs, *exp;

    incount = outcount = 0;
    inargs = outargs = 0;

    doing_outargs = parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1);

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        // call with zero arguments
        // insert rtn into expression tree
        return 1;
    }

    while (1) {

        if (!parse_expr(pctx, &arg, 0, 0)) {
            arg = 0; // set to a null argument
        }

        if (doing_outargs) {
            if (outargs == 0) {
                outargs = arg;
            } else {
                expr_next_set(lastarg, arg);
            }
            outcount += 1;
        } else {
            if (inargs == 0) {
                inargs = arg;
            } else {
                expr_next_set(lastarg, arg);
            }
            incount += 1;
        }

        lastarg = arg;

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

    // validate the arguments against the routine declaration, if we can
    // Insert the routine-call into the expression tree
    exp = expr_node_alloc(EXPTYPE_PRIM_RTNCALL, parser_curpos(pctx));
    expr_rtnaddr_set(exp, rtn);
    expr_inargs_set(exp, incount, inargs);
    expr_outargs_set(exp, outcount, outargs);
    *expp = exp;

    return 1;

} /* parse_arglist */

static expr_node_t *
lex_to_expr (lextype_t lt, lexeme_t *lex)
{
    expr_node_t *exp = 0;

    switch (lt) {
        case LEXTYPE_NUMERIC:
            exp = expr_node_alloc(EXPTYPE_PRIM_LIT, lexeme_textpos_get(lex));
            expr_litval_set(exp, lexeme_signedval(lex));
            break;
        case LEXTYPE_STRING: {
            int i;
            strdesc_t *text = lexeme_text(lex);
            unsigned long val = 0;
            exp = expr_node_alloc(EXPTYPE_PRIM_LIT, lexeme_textpos_get(lex));
            for (i = 0; i < text->len; i++) {
                val = val | (*(text->ptr+i) << (i*8));
            }
            expr_litval_set(exp, val);
            break;
        }
        case LEXTYPE_SEGMENT:
            exp = expr_node_alloc(EXPTYPE_PRIM_SEG, lexeme_textpos_get(lex));
            expr_segval_set(exp, lexeme_ctx_get(lex));
            break;
        default:
            break;
    }

    return exp;

} /* lex_to_expr */

static int
parse_fldref (parse_ctx_t pctx, stgctx_t stg, expr_node_t **expp) {

    expr_node_t *pos, *size, *exp;
    long signext = 0;

    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LANGLE, 0, 1)) {
        return 0;
    }
    pos = size = 0;
    if (!parse_expr(pctx, &pos, 0, 0)) {
        /* XXX error condition */
        pos = expr_node_alloc(EXPTYPE_NOOP, parser_curpos(pctx));
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        /* XXX error condition */
    }
    if (!parse_expr(pctx, &size, 0, 0)) {
        /* XXX error condition */
        size = expr_node_alloc(EXPTYPE_NOOP, parser_curpos(pctx));
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COMMA, 0, 1)) {
        if (!get_ctce(pctx, stg, &signext) || (signext != 0 &&
                                               signext != 1)) {
            /* XXX error condition */
            signext = 0;
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RANGLE, 0, 1)) {
        /* XXX error condition */
    }
    exp = expr_node_alloc(EXPTYPE_PRIM_FLDREF, parser_curpos(pctx));
    expr_fldref_addr_set(exp, *expp);
    expr_fldref_pos_set(exp, pos);
    expr_fldref_size_set(exp, size);
    expr_fldref_signext_set(exp, (signext != 0));
    *expp = exp;
    return 1;

} /* parse_fldref */

static int
parse_primary (parse_ctx_t pctx, lextype_t lt, lexeme_t *lex,
               expr_node_t **expp, int lstrok)
{
    expr_node_t *exp = 0;
    stgctx_t stg = parser_get_cctx(pctx);
    machinedef_t *mach = parser_get_machinedef(pctx);
    strdesc_t *codecomment = 0;
    lexseq_t labels;

    lexseq_init(&labels);
    if (lt == LEXTYPE_EXPRESSION) {
        exp = lexeme_ctx_get(lex);
        lexeme_free(lex);
        if (!expr_is_primary(exp)) {
            *expp = exp;
            return 1;
        }
    } else if (lt == LEXTYPE_KWD_CODECOMMENT) {
        lexeme_free(lex);
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt != LEXTYPE_STRING) {
            /* XXX error condition, but just assume it was missed */
        } else {
            codecomment = string_copy(0, lexeme_text(lex));
            lexeme_free(lex);
            lt = parser_next(pctx, QL_NORMAL, &lex);
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition, but assume it was missed */
        }
        while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
            lexseq_instail(&labels, lex);
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
        if (!parse_block(pctx, lt, &exp, codecomment, &labels)) {
            return 0;
        }
    } else if (lt == LEXTYPE_NAME_LABEL) {
        lexseq_instail(&labels, lex);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
            /* XXX error condition */
        }
        while (parser_expect(pctx, QL_NORMAL, LEXTYPE_NAME_LABEL, &lex, 1)) {
            lexseq_instail(&labels, lex);
            if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_COLON, 0, 1)) {
                /* XXX error condition */
            }
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
            if (!parse_block(pctx, lt, &exp, 0, &labels)) {
                return 0;
            }
        } else {
            /* XXX error condition */
            return 0;
        }
    } else if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
        lexeme_free(lex);
        if (!parse_block(pctx, lt, &exp, 0, &labels)) {
            return 0;
        }
    } else if (lt == LEXTYPE_NUMERIC || lt == LEXTYPE_SEGMENT ||
               lt == LEXTYPE_STRING) {
        if (lt == LEXTYPE_STRING && lexeme_textlen(lex) > mach->bpval/8) {
            strdesc_t *text = lexeme_text(lex);
            if (!lstrok) {
                /* XXX error condition */
            }
            text->len = mach->bpval/8;
        }
        exp = lex_to_expr(lt, lex);
        lexeme_free(lex);
    } else if (lt == LEXTYPE_NAME_FUNCTION) { // XXX Fix this
        exp = expr_node_alloc(EXPTYPE_EXECFUN, lexeme_textpos_get(lex));
        // XXX need to parse the entire function invocation here
        if (parse_fldref(pctx, stg, &exp)) {
            // and exp is updated to be a PRIM_FLDREF
            lexeme_free(lex);
            return 1;
        } else {
            expr_node_free(exp, stg);
            return 0; // executable-functions are not, by themselves, primaries
        }
    } else {
        return 0;
    }

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        expr_node_t *rtncall = 0;
        if (!parse_arglist(pctx, exp, &rtncall)) {
            /* XXX error condition */
            expr_node_free(exp, stg);
            return 0;
        }
        exp = rtncall;
    }

    // XXX Must also check for general routine calls

    // At this point, 'exp' is the primary expression.
    // If it's not already a field-ref, see if we've got one.
    if (expr_type(exp) != EXPTYPE_PRIM_FLDREF) {
        parse_fldref(pctx, stg, &exp);
    }

    *expp = exp;

    return 1;

} /* parse_primary */

static int
parse_op_expr (parse_ctx_t pctx, optype_t curop, expr_node_t **expp)
{
    expr_node_t *lhs = *expp;
    expr_node_t *thisnode, *rhs;
    optype_t op;
    int normal;

    if (expr_is_noop(lhs)) {
        if (!op_is_unary(curop)) {
            /* XXX error condition */
            // but try anyway?
        }
    } else {
        if (expr_is_ctrl(lhs)) {
            /* XXX error condition */
            // but try anyway
        }
    }

    rhs = 0;
    if (!parse_expr(pctx, &rhs, 1, 0) || rhs == 0) {
        // XXX error condition ?
        return 0;
    }

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

    if (normal) {
        thisnode = expr_node_alloc(EXPTYPE_OPERATOR, expr_textpos(rhs));
        expr_op_type_set(thisnode, curop);
        expr_op_lhs_set(thisnode, lhs);
        expr_op_rhs_set(thisnode, rhs);
        *expp = thisnode;
    } else {
        thisnode = expr_node_alloc(EXPTYPE_OPERATOR, expr_textpos(rhs));
        expr_op_type_set(thisnode, curop);
        expr_op_rhs_set(thisnode, expr_op_lhs(rhs));
        expr_op_lhs_set(rhs, thisnode);
        *expp = rhs;
    }

    return 1;

} /* parse_op_expr */

void
reduce_op_expr (stgctx_t stg, expr_node_t *node) {
    expr_node_t *lhs, *rhs;

    if (!expr_is_opexp(node)) {
        return;
    }

    lhs = expr_op_lhs(node);
    if (expr_is_opexp(lhs)) {
        reduce_op_expr(stg, lhs);
    }
    rhs = expr_op_rhs(node);
    if (expr_is_opexp(rhs)) {
        reduce_op_expr(stg, rhs);
    }
    // Check for unary +/- of a CTCE
    if (expr_is_noop(lhs)) {
        optype_t op = expr_op_type(node);
        if (op == OPER_UNARY_MINUS || op == OPER_UNARY_PLUS) {
            if (expr_type(rhs) == EXPTYPE_PRIM_LIT) {
                long result = (op == OPER_UNARY_MINUS ?
                               -expr_litval(rhs) : expr_litval(rhs));
                expr_node_free(rhs, stg);
                expr_node_free(lhs, stg);
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
                    result = leftval % rightval;
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
            expr_node_free(lhs, stg);
            expr_node_free(rhs, stg);
            expr_type_set(node, EXPTYPE_PRIM_LIT);
            expr_litval_set(node, result);
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
parse_expr (parse_ctx_t pctx, expr_node_t **expp,
            int recursed, int lstrok)
{
    lexeme_t *lex;
    lextype_t lt;
    optype_t op;
    machinedef_t *mach = parser_get_machinedef(pctx);
    stgctx_t stg = parser_get_cctx(pctx);
    int status = 0;

    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt == LEXTYPE_CTRL_WHILE || lt == LEXTYPE_CTRL_UNTIL) {
        lexeme_free(lex);
        return parse_wu_loop(pctx, lt, expp);
    }
    if (lt == LEXTYPE_CTRL_DO) {
        lexeme_free(lex);
        return parse_do_loop(pctx, lt, expp);
    }

    if (check_unary_op(lt, &op)) {
        lexeme_free(lex);
        status = parse_op_expr(pctx, op, expp);
        if (status && !recursed) {
            reduce_op_expr(stg, *expp);
        }
        return status;
    }

    if (parse_primary(pctx, lt, lex, expp, lstrok)) {
        lt = parser_next(pctx, QL_NORMAL, &lex);
        op = lextype_to_optype(lt, mach->addr_signed);
        if (op == OPER_NONE) {
            parser_insert(pctx, lex);
            return 1;
        }
        status = parse_op_expr(pctx, op, expp);
        if (status && !recursed) {
            reduce_op_expr(stg, *expp);
        }
        return status;
    }

    return status;

} /* parse_expr */

/*
 * expr_parse_next
 *
 * External interface to the expression parser.
 * Automatically binds literals and segments
 * to appropriate lexemes.
 */
lextype_t
expr_parse_next (parse_ctx_t pctx, lexeme_t **lexp,
                 int lstrok) {

    stgctx_t stg = parser_get_cctx(pctx);
    expr_node_t *exp = 0;

    if (!parse_expr(pctx, &exp, 0, lstrok)) {
        return parser_next(pctx, QL_NORMAL, lexp);
    }

    // Bind a CTCE to its value,
    // bind a segment reference to its value
    // otherwise, return an EXPRESSION lexeme
    if (expr_type(exp) == EXPTYPE_PRIM_LIT) {
        strdesc_t *str = string_printf(0, "%ld", expr_litval(exp));
        *lexp = parser_lexeme_create(pctx, LEXTYPE_NUMERIC, str);
        lexeme_val_setsigned(*lexp, expr_litval(exp));
        (*lexp)->type = LEXTYPE_NUMERIC;
        string_free(str);
        expr_node_free(exp, stg);
    } else if (expr_type(exp) == EXPTYPE_PRIM_SEG) {
        strdesc_t dsc = STRDEF("<segment>");
        *lexp = parser_lexeme_create(pctx, LEXTYPE_SEGMENT, &dsc);
        lexeme_ctx_set(*lexp, expr_segval(exp));
        (*lexp)->type = LEXTYPE_SEGMENT;
        expr_node_free(exp, stg);
    } else {
        strdesc_t dsc = STRDEF("<expression>");
        *lexp = parser_lexeme_create(pctx, LEXTYPE_EXPRESSION, &dsc);
        lexeme_ctx_set(*lexp, exp);
        (*lexp)->type = LEXTYPE_EXPRESSION;
    }

    return 1;
}

/*
 * parse_ctce
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
parse_ctce (parse_ctx_t pctx, lexeme_t **lexp)
{
    stgctx_t stg = parser_get_cctx(pctx);
    expr_node_t *exp = 0;
    int status = 0;

    if (!parse_expr(pctx, &exp, 0, 1)) {
        return 0;
    }

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
    expr_node_free(exp, stg);
    return status;

} /* parse_ctce */

/*
 * expr_parse_block
 *
 * External routine for parsing a block (specifically
 * for the MODULE declaration).  NB: no CODECOMMENT,
 * no labels.
 */
int
expr_parse_block (parse_ctx_t pctx, expr_node_t **blockexp)
{
    lextype_t lt;
    lexseq_t labels;

    lexseq_init(&labels);
    lt = parser_next(pctx, QL_NORMAL, 0);
    if (lt != LEXTYPE_DELIM_LPAR &&
        lt != LEXTYPE_EXP_DELIM_BEGIN) {
        /* XXX error condition */
        return 0;
    }

    return parse_block(pctx, lt, blockexp, 0, &labels);

} /* expr_parse_block */

/*
 * expr_dumpinfo
 *
 * Format a string with information about an expression
 */
strdesc_t *
expr_dumpinfo (expr_node_t *exp) {

    if (exp == 0) {
        return string_from_chrs(0, "{NULL}", 6);
    }
    switch (expr_type(exp)) {
        case EXPTYPE_PRIM_LIT:
            return string_printf(0, "LIT=%ld", expr_litval(exp));
        case EXPTYPE_PRIM_SEG: {
            strdesc_t *str = string_from_chrs(0, "SEG:", 4);
            return string_append(str, seg_dumpinfo(expr_segval(exp)));
        }
        case EXPTYPE_OPERATOR:
            return string_printf(0, "OP:%s", oper_name(expr_op_type(exp)));
        default:
            break;
    }
    return string_printf(0, "%s", exprtype_name(expr_type(exp)));
    
} /* expr_dumpinfo */

/*
 * parse_condexp
 *
 * IF test THEN consequent {ELSE alternative}
 */
static int
parse_condexp (void *pctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *orig, lexseq_t *result)
{
    expr_node_t *test, *cons, *alt, *exp;
    stgctx_t stg = parser_get_cctx(pctx);

    if (cs == COND_AWC || cs == COND_CWA) {
        lexeme_free(orig);
        return 1;
    }

    test = cons = alt = 0;
    if (!parse_expr(pctx, &test, 0, 0)) {
        /* XXX error condition */
        return -1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_THEN, 0, 1)) {
        /* XXX error condition */
        return -1;
    }
    if (!parse_expr(pctx, &cons, 0, 0)) {
        /* XXX error condition */
        return -1;
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_ELSE, 0, 1)) {
        if (!parse_expr(pctx, &alt, 0, 0)) {
            /* XXX error condition */
        }
        return -1;
    }

    // Optimize away constant tests
    if (expr_type(test) == EXPTYPE_PRIM_LIT) {
        if (expr_litval(test) & 1) {
            exp = cons;
        } else {
            exp = (alt == 0 ?
                   expr_node_alloc(EXPTYPE_NOOP, parser_curpos(pctx)) :
                   alt);
            expr_node_free(cons, stg);
        }
        expr_node_free(test, stg);
    } else {
        exp = expr_node_alloc(EXPTYPE_CTRL_COND, parser_curpos(pctx));
        expr_cond_test_set(exp, test);
        expr_cond_consequent_set(exp, cons);
        expr_cond_alternative_set(exp, alt);
    }
    orig->type = orig->boundtype = LEXTYPE_EXPRESSION;
    lexeme_ctx_set(orig, exp);
    return 0;

} /* parse_condexp */

static int
get_ctce (parse_ctx_t pctx, stgctx_t stg, long *valp)
{
    expr_node_t *exp = 0;

    if (!parse_expr(pctx, &exp, 0, 1)) {
        return 0;
    }

    if (expr_type(exp) == EXPTYPE_PRIM_LIT) {
        *valp = expr_litval(exp);
        expr_node_free(exp, stg);
        return 1;
    }
    expr_node_free(exp, stg);
    return 0;
} /* get_ctce */

/*
 * parse_case
 *
 * CASE exp FROM ctce TO ctce OF
 * SET
 *  [ctce { TO ctce } | INRANGE | OUTRANGE,...]: expression;
 * TES
 */
static int
parse_case (void *pctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t lt, condstate_t cs, lexeme_t *orig, lexseq_t *result)
{
    
    expr_node_t *caseidx;
    expr_node_t **cases, **unique, *exp;
    expr_node_t *outrange;
    int *todo;
    stgctx_t stg = parser_get_cctx(pctx);
    lexeme_t *lex;
    long lo, hi, ncases, i;
    int saw_inrange, saw_outrange, status;
    int unique_actions;

    if (cs == COND_AWC || cs == COND_CWA) {
        lexeme_free(orig);
        return 1;
    }

    if (!parse_expr(pctx, &caseidx, 0, 0)) {
        /* XXX error condition */
        return -1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        /* XXX error condition */
        return -1;
    }
    if (!get_ctce(pctx, stg, &lo)) {
        /* XXX error condition */
        return -1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        /* XXX error condition */
        return -1;
    }
    if (!get_ctce(pctx, stg, &hi)) {
        /* XXX error condition */
        return -1;
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
        return -1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        /* XXX error condition */
        return -1;
    }
    ncases = (hi - lo) + 1;
    cases = malloc(ncases*sizeof(expr_node_t *));
    unique = malloc(ncases*sizeof(expr_node_t *));
    memset(cases, 0, ncases*sizeof(expr_node_t *));
    memset(unique, 0, ncases*sizeof(expr_node_t *));
    todo = malloc(ncases*sizeof(int));
    saw_inrange = saw_outrange = 0;
    unique_actions = 0;
    status = 0;
    while (1) {
        int is_outrange = 0;
        memset(todo, 0, sizeof(int)*ncases);
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LBRACK, 0, 1)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, LEXTYPE_KWD_TES);
            status = -1;
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
        if (!parse_expr(pctx, &exp, 0, 0)) {
            /* XXX error condition */
            status = -1;
            break;
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

    if (status == 0) {
        for (i = 0; i < ncases && cases[i] != 0; i++);
        if (i < ncases) {
            /* XXX error condition - not all cases covered */
        }
        status = -1;
    }

    if (status == 0) {
        exp = expr_node_alloc(EXPTYPE_CTRL_CASE, parser_curpos(pctx));
        expr_case_bounds_set(exp, lo, hi);
        expr_case_outrange_set(exp, outrange);
        expr_case_actions_set(exp, cases);
        expr_case_index_set(exp, caseidx);
        orig->type = orig->boundtype = LEXTYPE_EXPRESSION;
        lexeme_ctx_set(lex, exp);
    } else {
        expr_node_free(outrange, stg);
        expr_node_free(caseidx, stg);
        while (unique_actions > 0) {
            expr_node_free(unique[--unique_actions], stg);
        }
    }

    free(unique);
    free(cases);
    free(todo);

    return status;
}
/*
 * parse_select
 *
 * SELECT{A|U}|SELECTONE{A|U} exp OF
 * SET
 *  [ expr { TO expr } | OTHERWISE | ALWAYS,...]: expr;
 * TES
 */
static int
parse_select (void *pctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t curlt, condstate_t cs, lexeme_t *orig, lexseq_t *result)
{
    stgctx_t stg = parser_get_cctx(pctx);
    machinedef_t *mach = parser_get_machinedef(pctx);
    expr_node_t *si, *exp, *lo, *hi;
    expr_node_t *selseq, *seqlast, *selectors, *sellast, *sel;

    if (!parse_expr(pctx, &si, 0, 0)) {
        /* XXX error condition */
        return -1;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OF, 0, 1)) {
        /* XXX error condition */
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_SET, 0, 1)) {
        /* XXX error condition */
    }
    selseq = seqlast = 0;
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
        sel = expr_node_alloc(EXPTYPE_SELECTOR, parser_curpos(pctx));
        while (1) {
            lo = hi = 0;
            if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_OTHERWISE, 0, 1)) {
                expr_selector_otherwise_set(sel, 1);
            } else if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_ALWAYS, 0, 1)) {
                expr_selector_always_set(sel, 1);
            } else if (parse_expr(pctx, &lo, 0, 0)) {
                if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
                    if (!parse_expr(pctx, &hi, 0, 0)) {
                        /* XXX error condition */
                    }
                }
                expr_selector_lohi_set(sel, lo, hi);
            } else {
                /* XXX error condition */
                expr_node_free(sel, stg);
                expr_node_free(selectors, stg);
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
        if (!parse_expr(pctx, &exp, 0, 1)) {
            /* XXX error condition */
        }
        if (selectors == 0) {
            expr_node_free(exp, stg);
        } else {
            expr_selector_action_set(selectors, exp);
            if (selseq == 0) {
                selseq = seqlast = selectors;
            } else {
                expr_next_set(seqlast, selectors);
                seqlast = selectors;
            }
        }
        if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            /* XXX error condition */
        }
    } /* outer loop */

    if (selseq == 0) {
        /* XXX error condition */
        expr_node_free(si, stg);
        return -1;
    }

    exp = expr_node_alloc(EXPTYPE_CTRL_SELECT, parser_curpos(pctx));
    expr_sel_index_set(exp, si);
    if (curlt == LEXTYPE_CTRL_SELECTONE ||
        curlt == LEXTYPE_CTRL_SELECTONEU ||
        curlt == LEXTYPE_CTRL_SELECTONEA) {
        expr_sel_oneonly_set(exp, 1);
    }
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
            expr_sel_cmptype_set(exp, (mach->addr_signed ?
                                       OPER_CMP_EQL : OPER_CMP_EQLU));
            break;
        default:
            break;
    }
    expr_sel_selectors_set(exp, selseq);
    orig->type = orig->boundtype = LEXTYPE_EXPRESSION;
    lexeme_ctx_set(orig, exp);

    return 0;
}
/*
 * parse_incrdecr
 *
 * INCR{A|U}|DECR{A|U} name {FROM exp} {TO exp} {BY exp}
 */
static int
parse_incrdecr (void *pctx, quotelevel_t ql, quotemodifier_t qm,
               lextype_t curlt, condstate_t cs, lexeme_t *orig, lexseq_t *result)
{
    stgctx_t stg = parser_get_cctx(pctx);
    machinedef_t *mach = parser_get_machinedef(pctx);
    expr_node_t *fromexp, *toexp, *byexp, *body, *exp;
    lextype_t lt;
    lexeme_t *lex;
    scopectx_t scope;

    fromexp = toexp = byexp = 0;
    scope = parser_scope_begin(pctx);
    lt = parser_next(pctx, QL_NAME, &lex);
    if (lexeme_boundtype(lex) != LEXTYPE_NAME) {
        /* XXX error condition */
        parser_scope_end(pctx);
        parser_insert(pctx, lex);
        return -1;
    }
    // XXX need to declare the name LOCAL here
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_FROM, 0, 1)) {
        if (!parse_expr(pctx, &fromexp, 0, 0)) {
            /* XXX error condition */
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_TO, 0, 1)) {
        if (!parse_expr(pctx, &toexp, 0, 0)) {
            /* XXX error condition */
        }
    }
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_KWD_BY, 0, 1)) {
        if (!parse_expr(pctx, &byexp, 0, 0)) {
            /* XXX error condition */
        }
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        /* XXX error condition */
    }
    if (!parse_expr(pctx, &body, 0, 0)) {
        expr_node_free(fromexp, stg);
        expr_node_free(toexp, stg);
        expr_node_free(byexp, stg);
        parser_scope_end(pctx);
        return -1;
    }
    parser_scope_end(pctx);
    exp = expr_node_alloc(EXPTYPE_CTRL_LOOPID, parser_curpos(pctx));
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
            expr_idloop_cmptype_set(exp, (mach->addr_signed ?
                                          OPER_CMP_GEQ : OPER_CMP_GEQU));
            break;
        case LEXTYPE_CTRL_INCRA:
            expr_idloop_cmptype_set(exp, (mach->addr_signed ?
                                          OPER_CMP_LEQ : OPER_CMP_LEQU));
            break;
        default:
            break;
    }
    orig->type = orig->boundtype = LEXTYPE_EXPRESSION;
    lexeme_ctx_set(orig, exp);
    return 0;
    
} /* parse_incrdecr */