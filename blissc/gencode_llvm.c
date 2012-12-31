/*
 *++
 *	File:			gencode_llvm.c
 *
 *	Abstract:		Generates LLVM IR.
 *
 *  Module description:
 *
 *       This module generates LLVM from the expression tree.
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		28-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */

#include <stdio.h>
#include "gencode.h"
#include "expression.h"
#include "symbols.h"
#include "machinedef.h"
#include "llvm-c/Core.h"
#include <stdlib.h>

// Dispatch table for expression code generators

typedef int (*exprgen_fn)(gencodectx_t gctx, expr_node_t *);

#define DOEXPTYPE(typ_) \
    static int gencode_expr_##typ_(gencodectx_t gctx, expr_node_t *exp);
DOEXPTYPES
#undef DOEXPTYPE
#define DOEXPTYPE(typ_) gencode_expr_##typ_,
static exprgen_fn exprgen_dispatch[] = {
    DOEXPTYPES
};
#undef DOEXPTYPE

// Module context

struct gencodectx_s {
    expr_ctx_t          ectx;
    stgctx_t            stg;
    machinedef_t        *mach;
    name_t              *modnp;
    LLVMContextRef      llvmctx;
    LLVMModuleRef       module;
    LLVMTypeRef         novalue_type;
    LLVMTypeRef         fullword_type;
    LLVMTypeRef         unit_type;
    LLVMValueRef        curfn;
    LLVMBuilderRef      builder;
};

// Utility functions

static void
namestring_from_dsc (char *buf, strdesc_t *dsc)
{
    size_t len = (dsc->len > NAME_SIZE-1 ? NAME_SIZE-1 : dsc->len);
    memcpy(buf, dsc->ptr, len);
    buf[len] = '\0';
}

LLVMTypeRef gendatatype (gencodectx_t gctx, data_attr_t *attr, seg_t *seg)
{
    machinedef_t *mach = gctx->mach;
    unsigned int units;
    
    if (attr->flags & SYM_M_REF) {
        return LLVMPointerType(gctx->unit_type, 0);
    }
    units = (seg == 0 ? attr->units : (unsigned int) seg_size(seg));
    if (units == 1) return gctx->unit_type;
    if (units == 0 || units == machine_scalar_units(mach)) {
        return gctx->fullword_type;
    }
    if (units < machine_scalar_units(mach) && units != 0) {
        return LLVMIntTypeInContext(gctx->llvmctx,
                                    units * machine_unit_bits(mach));
    }
    
    return LLVMArrayType(gctx->unit_type, units);
}

static LLVMTypeRef *
build_argtypes (gencodectx_t gctx, namereflist_t *argrefs)
{
    LLVMTypeRef *argtypes;
    nameref_t *arg;
    int i;
    
    if (namereflist_length(argrefs) == 0) {
        return 0;
    }
    argtypes = malloc(namereflist_length(argrefs)*sizeof(LLVMTypeRef));
    for (i = 0, arg = namereflist_head(argrefs);
         i < namereflist_length(argrefs); i++, arg = arg->tq_next) {
        data_attr_t *attr = datasym_attr(arg->np);
        argtypes[i] = gendatatype(gctx, attr, 0);
    }
    
    return argtypes;
}

void
set_argnames (gencodectx_t gctx, LLVMValueRef thisfn, namereflist_t *argrefs)
{
    LLVMValueRef *args;
    char namestr[NAME_SIZE];
    nameref_t *arg;
    int i;
    
    if (namereflist_length(argrefs) == 0) {
        return;
    }
    args = malloc(namereflist_length(argrefs)*sizeof(LLVMValueRef));
    LLVMGetParams(thisfn, args);
    for (i = 0, arg = namereflist_head(argrefs);
         i < namereflist_length(argrefs); i++, arg = arg->tq_next) {
        namestring_from_dsc(namestr, name_string(arg->np));
        LLVMSetValueName(args[i], namestr);
    }
    free(args);
}

static int
gencode_expr_NOOP (gencodectx_t gctx, expr_node_t *node)
{
    return 1;

} /* gencode_expr_NOOP */

static int
gencode_expr_PRIM_LIT (gencodectx_t gctx, expr_node_t *node)
{
    machinedef_t *mach = expr_machinedef(gctx->ectx);
    strdesc_t *str;
    LLVMValueRef val;

    str = expr_litstring(node);
    if (str == 0) {
        val = LLVMConstInt(gctx->fullword_type, expr_litval(node),
                           (machine_signext_supported(mach) ? 1 : 0));
    } else {
        val = LLVMConstStringInContext(gctx->llvmctx,
                                       str->ptr, str->len, 1);
    }
    expr_genref_set(node, val);

    return 1;

} /* gencode_expr_PRIM_LIT */

static int
gencode_expr_PRIM_SEG (gencodectx_t gctx, expr_node_t *node)
{
    name_t *np = expr_seg_name(node);
    char namestr[NAME_SIZE];
    LLVMValueRef val;

    namestring_from_dsc(namestr, name_string(np));

    switch (name_type(np)) {
        case LEXTYPE_NAME_DATA: {
            data_attr_t *attr = datasym_attr(np);
            val = LLVMBuildLoad(gctx->builder, datasym_genref(np), namestr);
            if (attr->flags & SYM_M_VOLATILE) {
                LLVMSetVolatile(val, 1);
            }
            break;
        }
        case LEXTYPE_NAME_ROUTINE: {
            val = LLVMBuildLoad(gctx->builder, rtnsym_genref(np), namestr);
            break;
        }
        default:
            expr_signal(gctx->ectx, STC__INTCMPERR,
                        "gencode_expr_PRIM_SEG");
            return 0;
            break;
    }
    expr_genref_set(node, val);
    return 1;
}

static int
gencode_expr_PRIM_FLDREF (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_PRIM_RTNCALL (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_PRIM_STRUREF (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_PRIM_BLK (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_OPERATOR (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_EXECFUN (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_COND (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_CASE (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_EXIT (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_RET (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_SELECTOR (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_SELECT (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_LOOPWU (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
static int
gencode_expr_CTRL_LOOPID (gencodectx_t gctx, expr_node_t *node)
{
    return 1;
}
// --- Begin API ---

/*
 * gencode_init
 *
 * Module initialization.
 */
gencodectx_t
gencode_init (logctx_t logctx, machinedef_t *mach, stgctx_t stg)
{
    gencodectx_t gctx = malloc(sizeof(struct gencodectx_s));

    if (gctx == 0) return 0;
    memset(gctx, 0, sizeof(struct gencodectx_s));
    gctx->stg = stg;
    gctx->mach = mach;
    gctx->llvmctx = LLVMContextCreate();
    if (gctx->llvmctx == 0) {
        log_signal(logctx, 0, STC__INTCMPERR, "gencode_init");
        free(gctx);
        return 0;
    }

    gctx->novalue_type = LLVMVoidTypeInContext(gctx->llvmctx);
    gctx->fullword_type = LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(mach));
    if (machine_scalar_units(mach) == 1) {
        gctx->unit_type = gctx->fullword_type;
    } else {
        gctx->unit_type = LLVMIntTypeInContext(gctx->llvmctx, machine_unit_bits(mach));
    }

    return gctx;
    
} /* gencode_init */

/*
 * gencode_routine_begin
 */
int
gencode_routine_begin (gencodectx_t gctx, name_t *np)
{
    LLVMValueRef thisfn = rtnsym_genref(np);
    LLVMBasicBlockRef entryblk;
    
    entryblk = LLVMAppendBasicBlockInContext(gctx->llvmctx, thisfn, "entry");
    gctx->curfn = thisfn;
    gctx->builder = LLVMCreateBuilderInContext(gctx->llvmctx);
    LLVMPositionBuilderAtEnd(gctx->builder, entryblk);

    return 1;
}

/*
 * gencode_routine_end
 */
int
gencode_routine_end (gencodectx_t gctx, name_t *np)
{
    LLVMValueRef thisfn = rtnsym_genref(np);
    expr_node_t *rtnexpr = rtnsym_expr(np);
    LLVMBasicBlockRef lastinfn;
    
    gctx->curfn = thisfn;
    lastinfn = LLVMGetLastBasicBlock(thisfn);
    LLVMPositionBuilderAtEnd(gctx->builder, lastinfn);
    return exprgen_dispatch[expr_type(rtnexpr)](gctx, rtnexpr);
}

/*
 * gencode_litsym
 */
int
gencode_litsym (gencodectx_t gctx, name_t *np)
{
    literal_attr_t *attr = litsym_attr(np);
    namectx_t namectx = scope_namectx(name_scope(np));
    char namestr[NAME_SIZE];
    name_t *gnp;
    LLVMTypeRef mytype;
    LLVMValueRef val;

    namestring_from_dsc(namestr, name_string(np));

    gnp = name_globalname(namectx, np);
    if (gnp == 0 || litsym_genref(gnp) == 0) {
        mytype = (attr->width == machine_scalar_bits(gctx->mach))
                 ? gctx->fullword_type
                 : LLVMIntTypeInContext(gctx->llvmctx, attr->width);
        val = LLVMConstInt(mytype, name_value_unsigned(np), (attr->flags & SYM_M_SIGNEXT) != 0);
        litsym_genref_set(np, val);
        if (gnp != 0) {
            LLVMValueRef gval;
            gval = LLVMAddGlobal(gctx->module, mytype, namestr);
            LLVMSetInitializer(gval, val);
            LLVMSetGlobalConstant(gval, 1);
            litsym_genref_set(gnp, gval);
        }
    } else {
        litsym_genref_set(np, litsym_genref(gnp));
    }

    return 1;

} /* gencode_litsym */


/*
 * gencode_datasym
 */
int
gencode_datasym (gencodectx_t gctx, name_t *np)
{
    namectx_t namectx = scope_namectx(name_scope(np));
    data_attr_t *attr = datasym_attr(np);
    seg_t *seg = datasym_seg(np);
    char namestr[NAME_SIZE];
    LLVMTypeRef mytype;
    LLVMValueRef val;

    if ((attr->flags & SYM_M_FORWARD) || seg == 0) {
        return 1;
    }
    mytype = gendatatype(gctx, attr, seg);
    namestring_from_dsc(namestr, name_string(np));
    if (seg != 0 && seg_type(seg) == SEGTYPE_STACK) {
        val = LLVMBuildAlloca(gctx->builder, mytype, namestr);
    } else if (seg == 0 || seg_type(seg) == SEGTYPE_STATIC) {
        val = LLVMAddGlobal(gctx->module, mytype, namestr);
        if (name_globalname(namectx, np) == 0) {
            LLVMSetLinkage(val, LLVMInternalLinkage);
        }
    }
    datasym_genref_set(np, val);

    return 1;
    
}

/*
 * gencode_rtnsym
 */
int
gencode_rtnsym (gencodectx_t gctx, name_t *rnp)
{
    routine_attr_t *attr = rtnsym_attr(rnp);
    char namestr[NAME_SIZE];
    LLVMTypeRef *argtypes;
    LLVMTypeRef thisfntype;
    LLVMValueRef thisfn;

    if (attr->flags & SYM_M_FORWARD) {
        return 1;
    }
    namestring_from_dsc(namestr, name_string(rnp));
    argtypes = build_argtypes(gctx, &attr->inargs);
    thisfntype = LLVMFunctionType((attr->flags & SYM_M_NOVALUE)
                                  ? gctx->novalue_type
                                  : gctx->fullword_type, argtypes,
                                  namereflist_length(&attr->inargs), 1);
    if (thisfntype == 0) {
        return 0;
    }
    thisfn = LLVMGetNamedFunction(gctx->module, namestr);
    if (thisfn == 0) {
        thisfn = LLVMAddFunction(gctx->module, namestr, thisfntype);
        if (name_globalname(expr_namectx(gctx->ectx), rnp) != 0) {
            LLVMSetLinkage(thisfn, LLVMExternalLinkage);
        } else {
            LLVMSetLinkage(thisfn, LLVMInternalLinkage);
        }
    }
    set_argnames(gctx, thisfn, &attr->inargs);
    rtnsym_genref_set(rnp, thisfn);

    return 1;

} /* gencode_rtnsym */

/*
 * gencode_module_begin
 */
int
gencode_module_begin (gencodectx_t gctx, void *exprctx, name_t *modnp)
{
    char modname[NAME_SIZE];

    gctx->modnp = modnp;
    gctx->ectx = exprctx;
    namestring_from_dsc(modname, name_string(modnp));
    gctx->module = LLVMModuleCreateWithNameInContext(modname, gctx->llvmctx);

    return (gctx->module != 0);

} /* gencode_module_begin */

int
gencode_module_end (gencodectx_t gctx, name_t *np)
{
    if (np != gctx->modnp) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "gencode_module_end");
    }
    
    LLVMDumpModule(gctx->module);
    LLVMDisposeModule(gctx->module);

    return 1;

} /* gencode_module_end */

