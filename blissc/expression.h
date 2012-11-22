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
#include "lexeme.h"

#undef DOEXPTYPE
#define DOEXPTYPES \
    DOEXPTYPE(PRIMARY) DOEXPTYPE(OPERATOR) \
    DOEXPTYPE(EXECFUN) DOEXPTYPE(CONTROL)
#define DOEXPTYPE(t_) EXPTYPE_##t_,
typedef enum {
    DOEXPTYPES
} exprtype_t;
#undef DOEXPTYPE
#define EXPTYPE_COUNT (EXPTYPE_CONTROL+1)

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

struct expr_node_s {
    struct expr_node_s *parent;
    struct expr_node_s *lhs, *rhs;
    exprtype_t          type;
    optype_t            op;
    lexeme_t            *lex;     // for lexical values
    struct expr_node_s  *seqnext; // for blocks
};
typedef struct expr_node_s expr_node_t;

struct expr_ctx_s {
    expr_node_t     *treehead;
};
typedef struct expr_ctx_s *expr_ctx_t;

const char *exprtype_name(exprtype_t type);
expr_node_t *expr_node_alloc(exprtype_t type);
void expr_node_free(expr_node_t *node);

static inline __unused expr_node_t *expr_node_left(expr_node_t *node) {
    return node->lhs;
}
static inline __unused expr_node_t *expr_node_right(expr_node_t *node) {
    return node->rhs;
}
static inline __unused exprtype_t expr_node_type(expr_node_t *node) {
    return node->type;
}
static inline __unused optype_t expr_node_optype(expr_node_t *node) {
    return (node->type == EXPTYPE_OPERATOR ? node->op : OPER_NONE);
}
static inline __unused lexeme_t *expr_node_lex(expr_node_t *node) {
    return node->lex;
}
static inline void expr_tree_insleft(expr_node_t *parent, expr_node_t *node) {
    parent->lhs = node;
    if (node != 0) node->parent = parent;
}
static inline void expr_tree_insright(expr_node_t *parent, expr_node_t *node) {
    parent->rhs = node;
    if (node != 0) node->parent = parent;
}

void expr_init (scopectx_t kwdscope);
int parse_Expression(parse_ctx_t pctx);
int parse_ctce(parse_ctx_t pctx, lexeme_t **lexp);

#endif
