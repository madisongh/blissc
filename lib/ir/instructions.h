#ifndef instructions_h__
#define instructions_h__
/*
 *++
 * instructions.h - BLISS IR instructions
 *
 * Copyright © 2014  Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "blissir_private.h"
#include "blissc/support/utils.h"

struct blissir_inst_branch_s {
    blissir_basicblockref_t basicblock;
};
struct blissir_inst_binop_s {
    blissir_binoptype_t     optype;
    blissir_valueref_t      lhs, rhs;
};
struct blissir_inst_call_s {
    blissir_routineref_t    callee;
    blissir_valref_seq_t    arglist;
};
struct blissir_inst_ret_s {
    blissir_valueref_t      retval;
};
struct blissir_inst_load_s {
    blissir_valueref_t      val;
};
struct blissir_inst_store_s {
    blissir_valueref_t      val, dest;
};
struct blissir_inst_extract_s {
    blissir_valueref_t      val, pos, width, signext;
};
struct blissir_inst_compare_s {
    blissir_compoptype_t    optype;
    blissir_valueref_t      lhs, rhs;
};
struct blissir_inst_phi_s {
    phiorg_seq_t            incoming;
};

struct blissir_instruction_s {
    TQ_ENT_FIELDS(struct blissir_instruction_s)
    blissir_insttype_t  type;
    strdesc_t           label;
    union {
        struct blissir_inst_branch_s  branch;
        struct blissir_inst_binop_s   binop;
        struct blissir_inst_call_s    call;
        struct blissir_inst_ret_s     ret;
        struct blissir_inst_load_s    load;
        struct blissir_inst_store_s   store;
        struct blissir_inst_extract_s extract;
        struct blissir_inst_compare_s compare;
        struct blissir_inst_phi_s     phi;
    } operands;
};
typedef struct blissir_instruction_s blissir_instruction_t;

#define siu static inline __attribute__((unused))
siu blissir_instruction_t *instr_next(blissir_instruction_t *inst) { return inst->tq_next; }
siu blissir_insttype_t instr_type(blissir_instruction_t *inst) { return inst->type; }
#undef siu

#endif /* instructions_h__ */
