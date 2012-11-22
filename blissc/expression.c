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
    NAMEDEF("SELECT", LEXTYPE_CTRL_SELECT, NAME_M_RESERVED),
    NAMEDEF("SELECTU", LEXTYPE_CTRL_SELECTU, NAME_M_RESERVED),
    NAMEDEF("SELECTA", LEXTYPE_CTRL_SELECTA, NAME_M_RESERVED),
    NAMEDEF("SELECTONE", LEXTYPE_CTRL_SELECTONE, NAME_M_RESERVED),
    NAMEDEF("SELECTONEU", LEXTYPE_CTRL_SELECTONEU, NAME_M_RESERVED),
    NAMEDEF("SELECTONEA", LEXTYPE_CTRL_SELECTONEA, NAME_M_RESERVED),
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

static int parse_expr(parse_ctx_t pctx, expr_node_t **expp, int recursed);

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
expr_node_alloc (exprtype_t type)
{
    expr_node_t *node;
    int i;

    if (freenodes == 0) {
        freenodes = malloc(ALLOC_QTY * sizeof(expr_node_t));
        if (freenodes == 0) {
            return 0;
        }
        for (i = 0, node = freenodes; i < ALLOC_QTY-1; i++, node++) {
            node->lhs = node + 1;
        }
        node->lhs = 0;
    }
    node = freenodes;
    freenodes = node->lhs;
    memset(node, 0, sizeof(expr_node_t));
    node->type = type;
    return node;

} /* expr_node_alloc */

void
expr_node_free (expr_node_t *node) {

    if (node == 0) {
        return;
    }
    expr_node_free(node->lhs);
    expr_node_free(node->rhs);
    memset(node, 0x77, sizeof(expr_node_t));
    node->lhs = freenodes;
    freenodes = node;
    if (node->type == EXPTYPE_PRIMARY) {
        lexeme_free(node->lex);
    }

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
}

int
parse_block (parse_ctx_t pctx, lextype_t curlt, expr_node_t **expp) {

    lextype_t lt;
    lexeme_t *lex;
    scopectx_t scope = 0;
    expr_node_t *exp = 0;
    expr_node_t *seq, *last;
    int expr_count;
    lextype_t closer = (curlt == LEXTYPE_EXP_DELIM_BEGIN ?
                        LEXTYPE_EXP_DELIM_END : LEXTYPE_DELIM_RPAR);


    lt = parser_next(pctx, QL_NORMAL, &lex);

    if (lt == closer) {
        lexeme_free(lex);
        *expp = 0; // null block
        return 1;
    }

    expr_count = 0;
    seq = last = 0;
    while (1) {
        if (lt >= LEXTYPE_DCL_MIN && lt <= LEXTYPE_DCL_MAX) {
            lexeme_free(lex);
            if (expr_count != 0) {
                /* XXX error condition */
                parser_skip_to_delim(pctx, closer);
                break;
            }
            if (scope == 0) {
                scope = parser_scope_begin(pctx);
            }
            parse_declaration(pctx, lt);
            lt = parser_next(pctx, QL_NORMAL, &lex);
            if (lt == closer) {
                lexeme_free(lex);
                break;
            }
            continue;
        } else {
            parser_insert(pctx, lex);
        }
        exp = 0;
        if (!parse_expr(pctx, &exp, 0)) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, closer);
            break;
        }
        expr_count += 1;
        if (seq == 0) {
            last = seq = exp;
        } else {
            last->seqnext = exp;
            last = exp;
        }
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == closer) {
            // use this expression as block's value
            lexeme_free(lex);
            break;
        }
        if (lt != LEXTYPE_DELIM_SEMI) {
            /* XXX error condition */
            parser_skip_to_delim(pctx, closer);
            break;
        }
        // discard the expression's value
        lexeme_free(lex);
        lt = parser_next(pctx, QL_NORMAL, &lex);
        if (lt == closer) {
            lexeme_free(lex);
            break;
        }
    }

    if (expr_count == 1 && scope == 0) {
        // simple compound expression, just return the expression
        *expp = seq;
    } else {
        if (scope != 0) {
            parser_scope_end(pctx);
        }
        exp = expr_node_alloc(EXPTYPE_PRIMARY);
        exp->lex = 0;
        exp->seqnext = seq;
        /* XXX revisit */
        *expp = exp;
    }

    return 1;

} /* parse_block */

static int
parse_wu_loop (parse_ctx_t pctx, lextype_t opener, expr_node_t **expp)
{
    expr_node_t *body, *test;

    if (!parse_expr(pctx, &test, 0)) {
        /* XXX error condition */
        return 0;
    }
    if (!parser_expect(pctx, QL_NORMAL, LEXTYPE_CTRL_DO, 0, 1)) {
        /* XXX error condition */
        return 0;
    }
    if (!parse_expr(pctx, &body, 0)) {
        /* XXX error condition */
        return 0;
    }

    return 1;

} /* parse_wu_loop */

static int
parse_do_loop (parse_ctx_t pctx, lextype_t opener, expr_node_t **expp)
{
    lextype_t lt;
    lexeme_t *lex;
    expr_node_t *body, *test;

    if (!parse_expr(pctx, &body, 0)) {
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

    if (!parse_expr(pctx, &test, 0)) {
        /* XXX error condition */
        return 0;
    }

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
parse_arglist (parse_ctx_t pctx, expr_node_t **expp)
{
    int inargs, outargs, doing_outargs;
    expr_node_t *rtn, *arg, *lastarg;

    rtn = *expp;
    inargs = outargs = 0;

    doing_outargs = parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1);

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
        // call with zero arguments
        // insert rtn into expression tree
        return 1;
    }

    lastarg = rtn;

    while (1) {

        if (!parse_expr(pctx, &arg, 0)) {
            arg = 0; // set to a null argument
        }

        if (doing_outargs) {
            expr_tree_insright(lastarg, arg);
            outargs += 1;
        } else {
            expr_tree_insleft(lastarg, arg);
            inargs += 1;
        }

        lastarg = arg;

        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_RPAR, 0, 1)) {
            break;
        }

        if (!doing_outargs &&
            parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_SEMI, 0, 1)) {
            doing_outargs = 1;
            lastarg = rtn;
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

    return 1;

} /* parse_arglist */

static int
parse_primary (parse_ctx_t pctx, lextype_t lt, lexeme_t *lex, expr_node_t **expp)
{
    expr_node_t *exp;
    machinedef_t *mach = parser_get_machinedef(pctx);

    if (lt == LEXTYPE_DELIM_LPAR || lt == LEXTYPE_EXP_DELIM_BEGIN) {
        lexeme_free(lex);
        if (!parse_block(pctx, lt, &exp)) {
            return 0;
        }
    } else if (lt == LEXTYPE_NUMERIC || lt == LEXTYPE_SEGMENT ||
        (lt == LEXTYPE_STRING && lexeme_textlen(lex) <= mach->bpval/8)) {
        exp = expr_node_alloc(EXPTYPE_PRIMARY);
        exp->lex = lex;
    } else if (lt == LEXTYPE_NAME_FUNCTION) { // XXX Fix this
        if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LANGLE, 0, 1)) {
            // parse the field-selector
            // and we have ourselves a primary
        } else {
            return 0; // executable-functions are not, by themselves, primaries
        }
    } else {
        return 0;
    }

    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LPAR, 0, 1)) {
        if (!parse_arglist(pctx, &exp)) {
            /* XXX error condition */
            expr_node_free(exp);
            return 0;
        }
    }

    // XXX Must also check for general routine call and codecomment

    // At this point, 'exp' is the primary expression

    // Any primary can take a field-selector
    if (parser_expect(pctx, QL_NORMAL, LEXTYPE_DELIM_LANGLE, 0, 1)) {
        // parse the field-selector
    }

    if (*expp == 0) {
        *expp = exp;
    } else {
        fprintf(stderr, "*** WTF?? ***\n");
        exit(EXIT_FAILURE);
    }

    return 1;

} /* parse_primary */

static int
parse_op_expr (parse_ctx_t pctx, optype_t curop, expr_node_t **expp)
{
    expr_node_t *lhs = *expp;
    expr_node_t *thisnode, *rhs;
    optype_t op;

    if (lhs == 0) {
        if (!op_is_unary(curop)) {
            /* XXX error condition */
            // but try anyway?
        }
    } else {
        if (expr_node_type(lhs) == EXPTYPE_CONTROL) {
            /* XXX error condition */
            // but try anyway
        }
    }

    rhs = 0;
    if (!parse_expr(pctx, &rhs, 1) || rhs == 0) {
        // XXX error condition ?
        return 0;
    }

    switch (expr_node_type(rhs)) {
        case EXPTYPE_CONTROL:
            /* XXX error condition */
            /* FALLTHROUGH */
        case EXPTYPE_PRIMARY:
        case EXPTYPE_EXECFUN:
            // normal case
            break;
        case EXPTYPE_OPERATOR:
            op = expr_node_optype(rhs);
            if (op_is_unary(op)) {
                if (op_priority(op) < op_priority(curop)) {
                    /* XXX error condition, but continue ? */
                }
                break; // normal case
            }
            if (op_priority(op) > op_priority(curop) ||
                (op_priority(op) == op_priority(curop) &&
                 op_is_r2l(op))) {
                break;
            }
            thisnode = expr_node_alloc(EXPTYPE_OPERATOR);
            thisnode->op = curop;
            expr_tree_insright(thisnode, expr_node_left(rhs));
            expr_tree_insleft(rhs, thisnode);
            *expp = rhs;
            return 1;
    }

    thisnode = expr_node_alloc(EXPTYPE_OPERATOR);
    thisnode->op = curop;
    expr_tree_insleft(thisnode, lhs);
    expr_tree_insright(thisnode, rhs);
    *expp = thisnode;
    return 1;

} /* parse_op_expr */

void
reduce_op_expr (expr_node_t *node) {
    expr_node_t *lhs, *rhs;

    if (expr_node_type(node) != EXPTYPE_OPERATOR) {
        return;
    }

    lhs = expr_node_left(node);
    if (lhs != 0 && expr_node_type(lhs) == EXPTYPE_OPERATOR) {
        reduce_op_expr(lhs);
    }
    rhs = expr_node_right(node);
    if (rhs != 0 && expr_node_type(rhs) == EXPTYPE_OPERATOR) {
        reduce_op_expr(rhs);
    }
    // Check for unary +/- of a CTCE
    if (lhs == 0) {
        optype_t op = expr_node_optype(node);
        if (op == OPER_UNARY_MINUS || op == OPER_UNARY_PLUS) {
            if (expr_node_type(rhs) == EXPTYPE_PRIMARY &&
                rhs->lex != 0 && lexeme_type(rhs->lex) == LEXTYPE_NUMERIC) {
                long result = (op == OPER_UNARY_MINUS ?
                               -lexeme_signedval(rhs->lex)
                               : lexeme_signedval(rhs->lex));
                node->type = EXPTYPE_PRIMARY;
                node->lex = lexeme_create(LEXTYPE_NUMERIC,
                                          string_printf(0, "%ld", result));
                lexeme_val_setsigned(node->lex, result);
                node->lex->type = LEXTYPE_NUMERIC;
                expr_node_free(rhs);
                node->rhs = 0;
            }
        }
        return;
    }

    // Check for operations on CTCEs
    if (expr_node_type(lhs) == EXPTYPE_PRIMARY && lhs->lex != 0 &&
        lexeme_type(lhs->lex) == LEXTYPE_NUMERIC) {
        long leftval = lexeme_signedval(lhs->lex);
        if (expr_node_type(rhs) == EXPTYPE_PRIMARY && rhs->lex != 0 &&
            lexeme_type(rhs->lex) == LEXTYPE_NUMERIC) {
            long rightval = lexeme_signedval(rhs->lex);
            long result;
            switch (expr_node_optype(node)) {
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
                    result = leftval / rightval;
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
            node->type = EXPTYPE_PRIMARY;
            node->lex = lexeme_create(LEXTYPE_NUMERIC,
                                      string_printf(0, "%ld", result));
            lexeme_val_setsigned(node->lex, result);
            node->lex->type = LEXTYPE_NUMERIC;
            expr_node_free(lhs);
            expr_node_free(rhs);
            node->lhs = node->rhs = 0;
        }
    }

} /* reduce_op_expr */

static int
parse_expr (parse_ctx_t pctx, expr_node_t **expp, int recursed)
{
    lexeme_t *lex;
    lextype_t lt;
    optype_t op;
    machinedef_t *mach = parser_get_machinedef(pctx);
    int status = 0;

    lt = parser_next(pctx, QL_NORMAL, &lex);
    if (lt == LEXTYPE_CTRL_WHILE || lt == LEXTYPE_CTRL_UNTIL) {
        lexeme_free(lex);
        status = parse_wu_loop(pctx, lt, expp);
    }
    if (lt == LEXTYPE_CTRL_DO) {
        lexeme_free(lex);
        status = parse_do_loop(pctx, lt, expp);
    }

    if (check_unary_op(lt, &op)) {
        lexeme_free(lex);
        status = parse_op_expr(pctx, op, expp);
        if (status && !recursed) {
            reduce_op_expr(*expp);
        }
    }
    if (parse_primary(pctx, lt, lex, expp)) {
        lt = parser_next(pctx, QL_NORMAL, &lex);
        op = lextype_to_optype(lt, mach->addr_signed);
        if (op == OPER_NONE) {
            parser_insert(pctx, lex);
            return 1;
        }
        status = parse_op_expr(pctx, op, expp);
        if (status && !recursed) {
            reduce_op_expr(*expp);
        }
    }

    return status;

} /* parse_expr */

int
parse_Expression (parse_ctx_t pctx) {

    expr_node_t *exp = 0;
    lexeme_t *lex;

    if (!parse_expr(pctx, &exp, 0)) {
        return 0;
    }

    if (expr_node_type(exp) == EXPTYPE_PRIMARY &&
        exp->lex != 0) {
        parser_insert(pctx, exp->lex);
        exp->lex = 0;
    } else {
        strdesc_t dsc = STRDEF("<expression>");
        lex = lexeme_create(LEXTYPE_EXPRESSION, &dsc);
        lexeme_ctx_set(lex, exp);
        parser_insert(pctx, lex);
    }

    expr_node_free(exp);
    return 1;
}

int
parse_ctce (parse_ctx_t pctx, lexeme_t **lexp)
{
    expr_node_t *exp = 0;
    lexeme_t *lex;

    if (!parse_expr(pctx, &exp, 0)) {
        return 0;
    }

    if (expr_node_type(exp) == EXPTYPE_PRIMARY &&
        exp->lex != 0) {
        lex = exp->lex;
        exp->lex = 0;
    } else {
        strdesc_t dsc = STRDEF("0");
        /* XXX error condition */
        lex = lexeme_create(LEXTYPE_NUMERIC, &dsc);
        lexeme_val_setsigned(lex, 0);
        lex->type = LEXTYPE_NUMERIC;
    }

    expr_node_free(exp);
    *lexp = lex;

    return 1;
}