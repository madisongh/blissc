//
//  lexeme.h
//  blissc
//
//  Created by Matthew Madison on 10/23/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_lexeme_h
#define blissc_lexeme_h

#include "strings.h"

typedef enum {
    QL_NORMAL, QL_NAME, QL_MACRO
} quotelevel_t;

typedef enum {
    QM_NONE, QM_QUOTE, QM_UNQUOTE, QM_EXPAND
} quotemodifier_t;

typedef enum {
    COND_NORMAL = 0,
    COND_CWC, COND_CWA,
    COND_AWC, COND_AWA
} condstate_t;


#undef DOLEXTYPE
#define DOLEXTYPES \
    DOLEXTYPE(NONE) \
    DOLEXTYPE(END) \
    DOLEXTYPE(UNBOUND) \
    DOLEXTYPE(NUMERIC) \
    DOLEXTYPE(STRING) \
    DOLEXTYPE(CSTRING) \
    DOLEXTYPE(OP_ADD) DOLEXTYPE(OP_SUB) DOLEXTYPE(OP_MUL) DOLEXTYPE(OP_DIV) \
    DOLEXTYPE(OP_MOD) \
    DOLEXTYPE(OP_ASSIGN) DOLEXTYPE(OP_FETCH) \
    DOLEXTYPE(OP_SHIFT) \
    DOLEXTYPE(OP_AND) DOLEXTYPE(OP_EQV) DOLEXTYPE(OP_OR) \
    DOLEXTYPE(OP_NOT) DOLEXTYPE(OP_XOR) \
    DOLEXTYPE(OP_EQL) DOLEXTYPE(OP_EQLU) DOLEXTYPE(OP_EQLA) \
    DOLEXTYPE(OP_GEQ) DOLEXTYPE(OP_GEQU) DOLEXTYPE(OP_GEQA) \
    DOLEXTYPE(OP_GTR) DOLEXTYPE(OP_GTRU) DOLEXTYPE(OP_GTRA) \
    DOLEXTYPE(OP_LSS) DOLEXTYPE(OP_LSSU) DOLEXTYPE(OP_LSSA) \
    DOLEXTYPE(OP_LEQ) DOLEXTYPE(OP_LEQU) DOLEXTYPE(OP_LEQA) \
    DOLEXTYPE(OP_NEQ) DOLEXTYPE(OP_NEQU) DOLEXTYPE(OP_NEQA) \
    DOLEXTYPE(DELIM_COMMA) \
    DOLEXTYPE(DELIM_SEMI) \
    DOLEXTYPE(DELIM_COLON) \
    DOLEXTYPE(DELIM_LPAR) \
    DOLEXTYPE(DELIM_RPAR) \
    DOLEXTYPE(DELIM_LBRACK) \
    DOLEXTYPE(DELIM_RBRACK) \
    DOLEXTYPE(DELIM_LANGLE) \
    DOLEXTYPE(DELIM_RANGLE) \
    DOLEXTYPE(LXF_ASCII) DOLEXTYPE(LXF_ASCIC) DOLEXTYPE(LXF_ASCIZ) \
    DOLEXTYPE(LXF_B) DOLEXTYPE(LXF_O) DOLEXTYPE(LXF_DECIMAL) DOLEXTYPE(LXF_X) \
    DOLEXTYPE(LXF_C) DOLEXTYPE(LXF_STRING) DOLEXTYPE(LXF_EXACTSTRING) \
    DOLEXTYPE(LXF_CHARCOUNT) DOLEXTYPE(LXF_CHAR) DOLEXTYPE(LXF_EXPLODE) \
    DOLEXTYPE(LXF_REMOVE) DOLEXTYPE(LXF_NAME) DOLEXTYPE(LXF_QUOTENAME) \
    DOLEXTYPE(LXF_NULL) DOLEXTYPE(LXF_IDENTICAL) DOLEXTYPE(LXF_ISSTRING) \
    DOLEXTYPE(LXF_REQUIRE) DOLEXTYPE(LXF_QUOTE) DOLEXTYPE (LXF_UNQUOTE) \
    DOLEXTYPE(LXF_EXPAND) DOLEXTYPE(LXF_IF) DOLEXTYPE(LXF_THEN) \
    DOLEXTYPE(LXF_ELSE) DOLEXTYPE (LXF_FI) DOLEXTYPE(LXF_DELIM_PERCENT) \
    DOLEXTYPE(LXF_EXITMACRO) DOLEXTYPE(LXF_EXITITER) DOLEXTYPE(LXF_ERRORMACRO) \
    DOLEXTYPE(LXF_REMAINING) DOLEXTYPE(LXF_COUNT) DOLEXTYPE(LXF_LENGTH) \
    DOLEXTYPE(NAME) \
    DOLEXTYPE(NAME_MACRO) DOLEXTYPE(NAME_MAC_PARAM) DOLEXTYPE(NAME_COMPILETIME) \
    DOLEXTYPE(NAME_LITERAL) DOLEXTYPE(NAME_DATA) DOLEXTYPE(NAME_STRUCTURE) \
    DOLEXTYPE(NAME_LINKAGE) DOLEXTYPE(NAME_ROUTINE) DOLEXTYPE(NAME_MODULE) \
    DOLEXTYPE(NAME_FUNCTION) DOLEXTYPE(NAME_LABEL) DOLEXTYPE(NAME_FIELD) \
    DOLEXTYPE(NAME_BIND) DOLEXTYPE(NAME_BUILTIN) DOLEXTYPE(NAME_PSECT) \
    DOLEXTYPE(DCL_MACRO) DOLEXTYPE(DCL_KEYWORDMACRO) \
    DOLEXTYPE(DCL_LITERAL) DOLEXTYPE(DCL_GLOBAL)

#define DOLEXTYPE(lt) LEXTYPE_##lt,
typedef enum {
    DOLEXTYPES
    LEXTYPE_COUNT           // MUST BE LAST
} lextype_t;
#undef DOLEXTYPE

#define LEXTYPE_MIN LEXTYPE_END
#define LEXTYPE_MAX (LEXTYPE_COUNT-1)
#define LEXTYPE_OP_MIN LEXTYPE_OP_ADD
#define LEXTYPE_OP_MAX LEXTYPE_OP_NEQA
#define LEXTYPE_DELIM_MIN LEXTYPE_DELIM_COMMA
#define LEXTYPE_DELIM_MAX LEXTYPE_DELIM_RANGLE
#define LEXTYPE_LXF_MIN LEXTYPE_LXF_ASCII
#define LEXTYPE_LXF_MAX LEXTYPE_LXF_LENGTH
#define LEXTYPE_NAME_MIN LEXTYPE_NAME
#define LEXTYPE_NAME_MAX LEXTYPE_NAME_PSECT
#define LEXTYPE_DCL_MIN LEXTYPE_DCL_MACRO
#define LEXTYPE_DCL_MAX LEXTYPE_DCL_GLOBAL

#define LEX_M_ALLOCATED (1<<0)

struct lexeme_s {
    struct lexeme_s *next;
    lextype_t        type;
    lextype_t        boundtype;
    strdesc_t        text;
    unsigned long    flags;
    unsigned long    numval;
    void            *extra;
};
typedef struct lexeme_s lexeme_t;

struct lexseq_s {
    lexeme_t    *head;
    lexeme_t    *tail;
    int          count;
};
typedef struct lexseq_s lexseq_t;

typedef int (*lextype_bind_fn)(void *pctx, quotelevel_t ql,
             quotemodifier_t qm, lextype_t lt, condstate_t cs,
             lexeme_t *orig, lexseq_t *result);

lexeme_t *lexeme_alloc(lextype_t type, const char *text, size_t len);
lexeme_t *lexeme_copy(lexeme_t *orig);
void lexeme_free(lexeme_t *lex);
lexeme_t *lexeme_create(lextype_t type, strdesc_t *dsc);
const char *lextype_name(lextype_t lt);

int lextype_register(lextype_t lt, lextype_bind_fn bindfn);
int lexeme_bind(void *ctx, quotelevel_t ql, quotemodifier_t qm,
                condstate_t cs, lexeme_t *lex, lexseq_t *result);

static inline __unused lexeme_t *lexeme_next (lexeme_t *lex) {
    return lex->next;
}
static inline __unused lextype_t lexeme_boundtype (lexeme_t *lex) {
    return (lex->type == LEXTYPE_UNBOUND ? lex->boundtype : lex->type);
}
static inline __unused lextype_t lexeme_type (lexeme_t *lex) {
    return lex->type;
}
static inline __unused long lexeme_signedval (lexeme_t *lex) {
    return (long) lex->numval;
}
static inline __unused unsigned long lexeme_unsignedval (lexeme_t *lex) {
    return lex->numval;
}
static inline __unused void lexeme_val_setsigned (lexeme_t *lex, long v) {
    lex->numval = (unsigned long) v;
}
static inline __unused void lexeme_val_setunsigned (lexeme_t *lex, unsigned long v) {
    lex->numval = v;
}
static inline __unused strdesc_t *lexeme_text (lexeme_t *lex) {
    return &lex->text;
}
static inline __unused void *lexeme_ctx_get (lexeme_t *lex) {
    return lex->extra;
}
static inline __unused void lexeme_ctx_set (lexeme_t *lex, void *p) {
    lex->extra = p;
}
static inline __unused void lexseq_init (lexseq_t *seq) {
    seq->head = seq->tail = 0; seq->count = 0;
}
static inline __unused int lexseq_emtpy (lexseq_t *seq) {
    return (seq->count == 0);
}
static inline __unused void lexseq_inshead (lexseq_t *seq, lexeme_t *l) {
    if (seq->count == 0) seq->tail = l;
    l->next = seq->head; seq->head = seq->tail = l; seq->count += 1;
}
static inline __unused void lexseq_instail (lexseq_t *seq, lexeme_t *l) {
    if (seq->count == 0) lexseq_inshead(seq, l);
    else { seq->tail->next = l; l->next = 0; seq->tail = l; seq->count += 1;}
}
static inline __unused void lexseq_append (lexseq_t *dst, lexseq_t *addon) {
    if (addon->count == 0) return;
    if (dst->count == 0) {
        dst->head = addon->head; dst->tail = addon->tail; dst->count = addon->count;
    } else {
        dst->tail->next = addon->head; dst->tail = addon->tail;
        dst->count += addon->count;
    }
    addon->head = addon->tail = 0; addon->count = 0;
}
static inline __unused void lexseq_prepend (lexseq_t *dst, lexseq_t *addon) {
    if (addon->count == 0) return;
    if (dst->count == 0) {
        dst->head = addon->head; dst->tail = addon->tail; dst->count = addon->count;
    } else {
        addon->tail->next = dst->head; dst->head = addon->head;
        dst->count += addon->count;
    }
    addon->head = addon->tail = 0; addon->count = 0;
}
static inline __unused lexeme_t *lexseq_remhead (lexseq_t *seq) {
    lexeme_t *l = seq->head;
    if (l == 0) return l;
    seq->head = l->next;
    l->next = 0;
    seq->count -= 1;
    return l;
}
static inline __unused lexeme_t *lexseq_remtail (lexseq_t *seq) {
    lexeme_t *l = seq->tail, *p = seq->head;
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
static inline __unused lexeme_t *lexseq_head (lexseq_t *seq) {
    return seq->head;
}
static inline __unused lexeme_t *lexseq_tail (lexseq_t *seq) {
    return seq->tail;
}
static inline __unused int lexseq_length (lexseq_t *seq) {
    return seq->count;
}
#endif
