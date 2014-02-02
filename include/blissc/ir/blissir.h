#ifndef blissir_h__
#define blissir_h__
/*
 *++
 *  blissir.h - BLISS IR definitions
 *
 *  Copyright © 2014, Matthew Madison
 *  All rights reserved.
 *  Distributed under license.  See LICENSE.TXT for details.
 *--
 */

#include "blissc/symbols.h"
#include "blissc/nametable.h"
#include "blissc/machinedef.h"
#include "blissc/support/strings.h"
#include "blissc/support/utils.h"

struct blissir_ctx_s;
typedef struct blissir_ctx_s *blissir_ctx_t;

struct blissir_module_s;
typedef struct blissir_module_s *blissir_moduleref_t;

struct blissir_routine_s;
typedef struct blissir_routine_s *blissir_routineref_t;

struct blissir_basicblock_s;
typedef struct blissir_basicblock_s *blissir_basicblockref_t;

struct blissir_value_s;
typedef struct blissir_valueref_s *blissir_valueref_t;

struct blissir_valref_s {
    TQ_ENT_FIELDS(struct blissir_valref_s)
    blissir_valueref_t   valp;
};
typedef struct blissir_valref_s blissir_valref_t;
typedef struct blissir_valref_seq_s {
    TQ_HDR_FIELDS(blissir_valref_t)
} blissir_valref_seq_t;
DEFINE_TQ_FUNCS(blissir_valref_seq, blissir_valref_seq_t, blissir_valref_t)

typedef enum {
    VALTYPE_LITERAL,
    VALTYPE_SEGADDR,
    VALTYPE_REGISTER,
    VALTYPE_CONDCODE,
} blissir_valtype_t;

struct blissir_builder_s;
typedef struct blissir_builder_s *blissir_builderref_t;

typedef enum {
    INST_BRANCH,
    INST_UNIOP,
    INST_BINOP,
    INST_NOOP,
    INST_CALL,
    INST_RET,
    INST_COMPARE,
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

struct blissir_instruction_s;
typedef struct blissir_instruction_s *blissir_instref_t;

struct blissir_phiorg_s {
    TQ_ENT_FIELDS(struct blissir_phiorg_s)
    blissir_basicblockref_t basicblock;
    blissir_valueref_t      value;
};
typedef struct blissir_phiorg_s blissir_phiorg_t;
typedef struct blissir_phiorg_seq_s {
    TQ_HDR_FIELDS(blissir_phiorg_t)
} blissir_phiorg_seq_t;
DEFINE_TQ_FUNCS(blissir_phiorg_seq, blissir_phiorg_seq_t, blissir_phiorg_t)


blissir_ctx_t blissir_init(machinedef_t *mach);
blissir_moduleref_t blissir_module_create(blissir_ctx_t ctx, name_t *np);

blissir_routineref_t blissir_routine_create(blissir_ctx_t ctx, name_t *np,
                                            routine_attr_t *attr);
blissir_routineref_t blissir_routine_find(blissir_ctx_t ctx, name_t *np);
int blissir_routine_begin(blissir_ctx_t ctx, name_t *np);
int blissir_routine_end(blissir_routineref_t rtn);

blissir_basicblockref_t blissir_basicblock_append(blissir_routineref_t rtn,
                                                  strdesc_t *label);
blissir_basicblockref_t blissir_basicblock_insert(blissir_routineref_t rtn,
                                                  blissir_basicblockref_t pred,
                                                  strdesc_t *label);
int blissir_basicblock_delete(blissir_basicblockref_t bb);
blissir_basicblockref_t blissir_basicblock_find(blissir_routineref_t rtn,
                                                strdesc_t *label);
blissir_builderref_t blissir_builder_create(blissir_ctx_t ctx,
                                            blissir_basicblockref_t bb);
int blissir_builder_setpos_atend(blissir_builderref_t builder,
                                 blissir_basicblockref_t bb);
strdesc_t *blissir_inst_getlabel(blissir_instref_t instr);
blissir_instref_t blissir_inst_branch(blissir_builderref_t builder,
                                      blissir_basicblockref_t dest,
                                      strdesc_t *label);
blissir_valref_t  blissir_inst_uniop(blissir_builderref_t builder,
                                     blissir_unioptype_t op,
                                     blissir_valueref_t value,
                                     strdesc_t *label);
blissir_valref_t blissir_inst_binop(blissir_builderref_t builder,
                                    blissir_binoptype_t op,
                                    blissir_valueref_t lhs,
                                    blissir_valueref_t rhs,
                                    strdesc_t *label);
blissir_instref_t blissir_inst_noop(blissir_builderref_t builder, strdesc_t *label);
blissir_valref_t blissir_inst_call(blissir_builderref_t builder,
                                   blissir_routineref_t callee,
                                   blissir_valref_seq_t *arglist,
                                   strdesc_t *label);
blissir_instref_t blissir_inst_ret(blissir_builderref_t builder,
                                   blissir_valueref_t retval,
                                   strdesc_t *label);
blissir_valueref_t blissir_inst_compare(blissir_builderref_t builder,
                                        blissir_compoptype_t op,
                                        blissir_valueref_t lhs,
                                        blissir_valueref_t rhs,
                                        strdesc_t *label);
blissir_valueref_t blissir_inst_load(blissir_builderref_t builder,
                                     blissir_valueref_t loc,
                                     strdesc_t *label);
blissir_instref_t blissir_inst_store(blissir_builderref_t builder,
                                     blissir_valueref_t val,
                                     blissir_valueref_t loc,
                                     strdesc_t *str);
blissir_valueref_t blissir_inst_extract(blissir_builderref_t builder,
                                        blissir_valueref_t val,
                                        blissir_valueref_t pos,
                                        blissir_valueref_t size,
                                        blissir_valueref_t signext,
                                        strdesc_t *str);
blissir_valueref_t blissir_inst_phi(blissir_builderref_t builder,
                                    blissir_phiorg_seq_t *incoming,
                                    strdesc_t *str);

#endif /* blissir_h__ */
