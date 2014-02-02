#ifndef instructions_h__
#define instructions_h__
/*
 *++
 * blissir/instructions.h - BLISS IR instructions
 *
 * Copyright © 2014  Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "blissir.h"

struct blissir_inst_branch_s;
struct blissir_inst_binop_s;
struct blissir_inst_noop_s;
struct blissir_inst_call_s;
struct blissir_inst_ret_s;
struct blissir_inst_load_s;
struct blissir_inst_store_s;
struct blissir_inst_extract_s;
struct blissir_inst_compare_s;
struct blissir_inst_phi_s;

typedef enum {
    INST_BRANCH,
    INST_UNIOP,
    INST_BINOP,
    INST_NOOP,
    INST_CALL,
    INST_RET,
    INST_LOAD,
    INST_STORE,
    INST_EXTRACT,
    INST_PHI,
} blissir_insttype_t;

typedef enum {
    UNIOP_NEG,
    UNIOP_NOT
} blissir_unioptype_t;

typedef enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_REM,
    BINOP_SHIFT,
    BINOP_AND,
    BINOP_OR,
    BINOP_XOR,
    BINOP_EQV,
} blissir_binoptype_t;

typedef enum {
    COMPOP_EQL,
    COMPOP_LSS,
    COMPOP_LEQ,
    COMPOP_GTR,
    COMPOP_GEQ,
    COMPOP_NEQ,
    COMPOP_EQLU,
    COMPOP_LSSU,
    COMPOP_LEQU,
    COMPOP_GTRU,
    COMPOP_GEQU,
    COMPOP_NEQU,
} blissir_compoptype_t;

struct blissir_instruction_s {
    blissir_insttype_t type;
    union {
        struct blissir_inst_branch_s  branch;
        struct blissir_inst_binop_s   binop;
        struct blissir_inst_noop_s    noop;
        struct blissir_inst_call_s    call;
        struct blissir_inst_ret_s     ret;
        struct blissir_inst_load_s    load;
        struct blissir_inst_store_s   store;
        struct blissir_inst_extract_s extract;
        struct blissir_inst_compare_s compare;
        struct blissir_inst_phi_s     phi;
    } inst;
};
typedef struct blissir_instruction_s blissir_instruction_t;

#define siu static inline __attribute__((unused))
siu blissir_insttype_t blissir_instruction_type(blissir_instruction_t *i) { return i->type; }
#undef siu

#endif /* instructions_h__ */
