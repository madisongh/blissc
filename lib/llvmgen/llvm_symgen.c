/*
 *++
 * llvm_symgen.c - Symbol generation for the LLVM back-end.
 *
 *
 * This module generates LLVM for symbols - literals,
 * data segments, BINDs, routines, labels, psects, and
 * modules.
 *
 * Besides generating the symbols themselves, this module
 * also handles generating addresses for the symbols needed
 * in fetch and assignment expressions.
 *
 * The front-end's symbols module has hooks that allow the
 * code generator to reserve additional memory for each
 * symbol, for tracking purposes.  We use this for stashing
 * the LLVM values for symbols, saving us an extra lookup
 * (and allowing for cases, like BINDs, where the BLISS name
 * doesn't necessarily map to an LLVM name).
 *
 * Copyright © 2013, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include "llvmgen.h"

// Upper bound on the number of expressions we'll process in
// a PRESET attribute.
#define LLVMGEN_K_MAXPRESETS 128

/*
 * Tracking structures attached to symbols, used with
 * the sym_genspace hooks.
 */
struct llvm_litsym_s {
    LLVMValueRef        value;
};
typedef struct llvm_litsym_s llvm_litsym_t;

struct llvm_datasym_s {
    LLVMValueRef        value;
    unsigned int        flags;
    llvm_stgclass_t     vclass;
    int                 deref;
};
typedef struct llvm_datasym_s llvm_datasym_t;

struct llvm_rtnsym_s {
    LLVMTypeRef         type;
    LLVMTypeRef         returntype;
    LLVMValueRef        func;
};
typedef struct llvm_rtnsym_s llvm_rtnsym_t;

struct llvm_label_s {
    llvm_btrack_t       *btrack;
};
typedef struct llvm_label_s llvm_label_t;

struct llvm_module_s {
    LLVMModuleRef        module;
};
typedef struct llvm_module_s llvm_module_t;

/*
 * machine_psects_init
 *
 * Set up the default PSECTs and their predeclared names.
 * XXX this is for Mach-O; needs to be selectable by target
 * object format.
 */
void
machine_psects_init (machinedef_t *mach, void *scope) {

    scopectx_t kwdscope = scope;
    machine_ctx_t mctx = machine_context(mach);
    name_t *np;
    strdesc_t datadata =   STRDEF("__DATA,__data");
    strdesc_t textconst =  STRDEF("__TEXT,__const");
    strdesc_t texttext =   STRDEF("__TEXT,__text");
    strdesc_t dotdata =    STRDEF(".data");
    strdesc_t dottext =    STRDEF(".text");

    np = psect_declare(kwdscope, (mctx->is_macho ? &datadata : &dotdata),
                       PSECT_M_ATTR_WRITE, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_OWN, np);
    scope_sclass_psectname_set(kwdscope, SCLASS_GLOBAL, np);
    np = psect_declare(kwdscope, (mctx->is_macho ? &textconst : &dottext), 0, 0);
    scope_sclass_psectname_set(kwdscope, SCLASS_PLIT, np);
    if (mctx->is_macho) {
        np = psect_declare(kwdscope, &texttext, PSECT_M_ATTR_EXEC, 0);
    } // otherwise, just reuse the text psect name
    scope_sclass_psectname_set(kwdscope, SCLASS_CODE, np);

} /* machine_psects_init */

/*
 * llvmgen_label_btrack
 *
 * Returns the pointer to the branch-tracking structure
 * associated with a label.  Used for handling LEAVE expressions.
 */
llvm_btrack_t *
llvmgen_label_btrack (name_t *np)
{
    llvm_label_t *lbl = sym_genspace(np);
    if (name_type(np) != LEXTYPE_NAME_LABEL) return 0;
    return lbl->btrack;

} /* llvmgen_label_btrack */

/*
 * llvmgen_label_btrack_set
 *
 * Sets the branch tracking context for a label.
 */
void
llvmgen_label_btrack_set (name_t *np, llvm_btrack_t *bt)
{
    llvm_label_t *lbl = sym_genspace(np);
    if (name_type(np) == LEXTYPE_NAME_LABEL) {
        lbl->btrack = bt;
    }

} /* llvmegen_label_btrack_set */

/*
 * litsym_generator
 *
 * Generates LLVM for a [GLOBAL] LITERAL.
 *
 * XXX Not sure that the way LLVM handles global constants
 * exactly matches the semantics the BLISS LRM describes.
 *
 */
static int
litsym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    literal_attr_t *attr = litsym_attr(np);
    llvm_litsym_t *ll = p;
    llvm_litsym_t *gll = 0;
    namectx_t namectx = scope_namectx(name_scope(np));
    name_t *gnp;
    LLVMTypeRef type;

    gnp = name_globalname(namectx, np);
    if (gnp != 0) {
        gll = sym_genspace(gnp);
        if (gll->value != 0) {
            ll->value = gll->value;
            return 1;
        }
    }
    type = LLVMIntTypeInContext(gctx->llvmctx, attr->width);
    if ((attr->flags & SYM_M_SIGNEXT) != 0) {
        ll->value = LLVMConstInt(type, name_value_signed(np), 1);
    } else {
        ll->value = LLVMConstInt(type, name_value_unsigned(np), 0);
    }
    if (gnp != 0 && gll != 0) {
        LLVMValueRef globalval;
        gll->value = ll->value;
        globalval = LLVMAddGlobal(gctx->module, type, name_azstring(np));
        LLVMSetInitializer(globalval, gll->value);
        LLVMSetGlobalConstant(globalval, 1);
    }

    return 1;

} /* litsym_generator */

/*
 * initializer_arraysize
 *
 * Calculates the size of the array of LLVMValueRefs needed for
 * setting up an initializer.
 */
static unsigned int
initializer_arraysize (initval_t *ivlist)
{
    initval_t *iv;
    unsigned int count = 0;

    for (iv = ivlist; iv != 0; iv = iv->next) {
        if (iv->type == IVTYPE_LIST) {
            unsigned int thiscount = initializer_arraysize(iv->data.listptr);
            count += iv->repcount * thiscount;
        } else {
            count += iv->repcount;
        }
    }

    return count;

} /* initializer_arraysize */

/*
 * llvmgen_initializer
 *
 * Generates an array of LLVMValueRefs populated with the values
 * for an initializer.  Used for INITIAL attribute handling.
 */
static LLVMValueRef
llvmgen_initializer (gencodectx_t gctx, initval_t *ivlist, unsigned int padcount, LLVMValueRef **arrp)
{
    unsigned int arrsize;
    unsigned int bpunit = machine_unit_bits(gctx->mach);
    LLVMValueRef *valarr, *valp, *lastvalp, oneval, initconst;
    initval_t *iv;
    int allsametype = 1;

    valarr = (arrp == 0 ? 0 : *arrp);
    if (valarr == 0) {
        arrsize = initializer_arraysize(ivlist) + padcount;
        if (arrsize > 1) {
            valarr = malloc(arrsize * sizeof(LLVMValueRef));
            memset(valarr, 0, arrsize * sizeof(LLVMValueRef));
        } else {
            valarr = &oneval;
        }
    }
    for (iv = ivlist, valp = valarr, lastvalp = 0; iv != 0; iv = iv->next) {
        int count = iv->repcount;
        LLVMValueRef thisval = 0;
        switch (iv->type) {
            case IVTYPE_LIST: {
                LLVMValueRef *savep = valp;
                LLVMValueRef *srcp, *dstp;

                llvmgen_initializer(gctx, iv->data.listptr, 0, &valp);
                dstp = valp;
                // Handle (> 1) repeat count by appending copies of the pointer(s) just inserted
                while (--count > 0) {
                    for (srcp = savep; srcp < valp; srcp++) *dstp++ = *srcp;
                }
                valp = dstp;
                // Compare first here against the last iteration of the loop
                if (allsametype && lastvalp != 0 && LLVMTypeOf(*lastvalp) != LLVMTypeOf(*savep)) {
                    allsametype = 0;
                }
                lastvalp = savep;
                // Now check all of the values just inserted
                while (allsametype && savep != valp) {
                    if (savep != valarr && LLVMTypeOf(*savep) != LLVMTypeOf(*(savep-1))) {
                        allsametype = 0;
                    }
                    savep += 1;
                }
                break;
            }
            case IVTYPE_STRING:
                thisval = LLVMConstStringInContext(gctx->llvmctx, iv->data.string->ptr,
                                                 iv->data.string->len, 1);
                break;
            case IVTYPE_SCALAR:
                thisval = LLVMConstInt(LLVMIntTypeInContext(gctx->llvmctx,
                                                          iv->data.scalar.width * bpunit),
                                     iv->data.scalar.value, iv->data.scalar.signext);
                break;
            case IVTYPE_EXPR_EXP: {
                expr_node_t *exp = iv->data.scalar.expr;
                thisval = llvmgen_expression(gctx, exp, 0);
                break;
            }
        }
        if (iv->type == IVTYPE_STRING || iv->type == IVTYPE_SCALAR ||
            iv->type == IVTYPE_EXPR_EXP) {
            LLVMValueRef *dstp = valp;
            while (count-- > 0) *dstp++ = thisval;
            if (allsametype && lastvalp != 0 && LLVMTypeOf(thisval) != LLVMTypeOf(*lastvalp)) {
                allsametype = 0;
            }
            lastvalp = valp;
            valp = dstp;
        }
    }

    if (arrp != 0) {
        *arrp = valp;
        return 0;
    }

    if (padcount > 0) {
        LLVMTypeRef utype = LLVMIntTypeInContext(gctx->llvmctx, bpunit);
        if (allsametype && LLVMTypeOf(*valarr) != utype) {
            allsametype = 0;
        }
        while (padcount-- > 0) {
            *valp++ = LLVMConstNull(utype);
        }
    }

    if (arrsize == 1) {
        return oneval;
    }

    if (allsametype) {
        initconst = LLVMConstArray(LLVMTypeOf(*valarr), valarr, arrsize);
    } else {
        initconst = LLVMConstStructInContext(gctx->llvmctx, valarr, arrsize, 1);
    }
    free(valarr);
    return initconst;

} /* llvmgen_initializer */

/*
 * llvmgen_presetter
 *
 * Sets up an initializer for a PRESET attribute.  This is more complicated
 * than INITIAL handling, since we have to order the initializers here by
 * offset from the base address.  Once we have computed all of the initializers
 * and put them in the correct order, we can generate an array of LLVMValueRefs
 * and create the initializer.
 */
static LLVMValueRef
llvmgen_presetter (gencodectx_t gctx, initval_t *ivlist, unsigned int padding)
{
    machinedef_t *mach = gctx->mach;
    unsigned int bpunit = machine_unit_bits(mach);
    unsigned int bpaddr = machine_addr_bits(mach);
    struct preset_s {
        unsigned int o, p, s, e;
        LLVMValueRef val;
    } pcells[LLVMGEN_K_MAXPRESETS];
    struct preset_s *parr[LLVMGEN_K_MAXPRESETS] = { 0 };
    unsigned int curoff, curbitpos, arrsize;
    int pcount, i;
    initval_t *iv;
    LLVMValueRef *varr, *valp, oneval, initconst;

    if (bpunit == 0 || bpaddr == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_presetter[0]");
        return 0;
    }
    pcount = 0;
    for (iv = ivlist; iv != 0 && pcount < LLVMGEN_K_MAXPRESETS; iv = iv->next) {
        expr_node_t *exp = iv->preset_expr;
        struct preset_s *this = &pcells[pcount];
        if (expr_type(exp) == EXPTYPE_PRIM_STRUREF) {
            exp = expr_struref_accexpr(exp);
        }
        if (expr_type(exp) == EXPTYPE_PRIM_SEG) {
            this->o = (unsigned int) expr_seg_offset(exp);
            this->p = 0;
            this->s = expr_seg_width(exp);
            this->e = expr_seg_signext(exp);
        } else if (expr_type(exp) == EXPTYPE_PRIM_FLDREF) {
            this->o = (unsigned int) expr_seg_offset(expr_fldref_addr(exp));
            this->p = (unsigned int) expr_litval(expr_fldref_pos(exp));
            // Everything here is in units, so fix any oversized positions
            if (this->p > bpunit) {
                this->o += this->p / bpunit;
                this->p = this->p % bpunit;
            }
            this->s = (unsigned int) expr_litval(expr_fldref_size(exp));
            this->e = expr_fldref_signext(exp);
        } else {
            // Should never get here
            expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_presetter[1]");
            continue;
        }
        exp = iv->data.scalar.expr;
        if (expr_type(exp) == EXPTYPE_PRIM_LIT) {
            this->val = LLVMConstInt(LLVMIntTypeInContext(gctx->llvmctx, this->s),
                                     expr_litval(exp), this->e);
        } else if (expr_type(exp) == EXPTYPE_PRIM_SEG) {
            long offset = expr_seg_offset(exp);
            LLVMValueRef v, o;
            if (this->s != bpaddr) {
                expr_signal(gctx->ectx, STC__PRINVADSZ);
                continue;
            }
            v = llvmgen_segaddress(gctx, expr_seg_name(exp), 0, 0);
            if (offset == 0) {
                this->val = v;
            } else {
                unsigned int bpval = machine_scalar_bits(gctx->mach);
                LLVMTypeRef inttype = LLVMIntTypeInContext(gctx->llvmctx, bpval);
                LLVMTypeRef unittype = LLVMIntTypeInContext(gctx->llvmctx, bpunit);
                LLVMTypeRef vtype = LLVMGetElementType(LLVMTypeOf(v));
                o = LLVMConstInt(inttype, offset, 1);
                if (LLVMGetTypeKind(vtype) != LLVMArrayTypeKind ||
                    LLVMGetElementType(vtype) != unittype) {
                    vtype = LLVMPointerType(LLVMArrayType(unittype, (unsigned int)(offset+1)), 0);
                    v = LLVMConstPointerCast(v, vtype);
                }
                this->val = LLVMConstGEP(v, &o, 1);
            }
        } else {
            expr_signal(gctx->ectx, STC__PRBADVALUE);
            continue;
        }
        // Insert in order by offset+position
        for (i = 0; i < pcount; i++) {
            if (this->o < parr[i]->o ||
                (this->o == parr[i]->o && this->p < parr[i]->p)) {
                break;
            }
        }
        // If we are inserting into the middle of the list, make room
        if (i < pcount) {
            int j;
            for (j = pcount+1; j > i; j--) {
                parr[j] = parr[j-1];
            }
        }
        parr[i] = this;
        pcount += 1;

    } /* loop through ivlist */

    // Signal the user if we ran out of preset cells
    if (iv != 0) {
        expr_signal(gctx->ectx, STC__EXCPRLIMIT);
    }

    if (pcount == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_presetter[2]");
        return 0;
    }

    // At this point, the parr[] array contains a list of pointers
    // to the initialization cells, in order by offset/bit-position within
    // the memory block.  We can now compute the number of initializer
    // values we need, including any gaps that need to be zero-filled.
    arrsize = 0;
    curoff = curbitpos = 0;
    for (i = 0; i < pcount; i++) {
        struct preset_s *this = parr[i];
        if (curoff > this->o) {
            // XXX punt on overlaps
            expr_signal(gctx->ectx, STC__PROVERLAP);
            parr[i] = 0; // to skip it
            continue;
        }
        if (this->o > curoff) {
            // need a zero-fill up to the current offset, plus one
            // for the bits up to the field's bit position, if non-zero,
            // plus one for padding the previous value if its size
            // was not an integral number of units
            arrsize += 1 + (this->p == 0 ? 0 : 1) + (curbitpos == 0 ? 0 : 1);
        } else {
            // check for overlap from the last field to this one
            // XXX for now, just punt on these
            if (curbitpos > this->p) {
                expr_signal(gctx->ectx, STC__PROVERLAP);
                parr[i] = 0;  // to skip it later
                continue;
            }
            if (this->p > curbitpos) {
                arrsize += 1; // need a zero-fill up to the current bit position
            }
        }
        curoff = this->o;
        curbitpos = this->p + this->s;
        if (curbitpos >= bpunit) {
            curoff += curbitpos / bpunit;
            curbitpos = curbitpos % bpunit;
        }
        arrsize += 1; // for the value itself
    }

    // Fill out the last unit, if needed
    if (curbitpos != 0) {
        arrsize += 1;
    }
    if (padding != 0) {
        arrsize += 1;
    }

    if (arrsize == 1) {
        varr = &oneval;
    } else {
        varr = malloc(arrsize * sizeof(LLVMValueRef));
        memset(varr, 0, arrsize * sizeof(LLVMValueRef));
    }

    curoff = curbitpos = 0;
    for (valp = varr, i = 0; i < pcount; i++) {
        struct preset_s *this = parr[i];
        if (this == 0) continue;
        if (this->o > curoff) {
            if (curbitpos != 0) {
                *valp++ = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx,
                                                             bpunit - curbitpos));
            }
            *valp++ = LLVMConstNull(LLVMArrayType(LLVMInt8TypeInContext(gctx->llvmctx),
                                                  this->o - curoff));
            curbitpos = 0;
        }
        if (this->p > curbitpos) {
            *valp++ = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx,
                                                         this->p - curbitpos));
        }
        *valp++ = this->val;
        curoff = this->o;
        curbitpos = this->p + this->s;
        if (curbitpos >= bpunit) {
            curoff += curbitpos / bpunit;
            curbitpos = curbitpos % bpunit;
        }
    }
    if (curbitpos != 0) {
        *valp++ = LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx, bpunit - curbitpos));
    }
    if (padding != 0) {
        *valp++ = LLVMConstNull(LLVMArrayType(LLVMIntTypeInContext(gctx->llvmctx, bpunit),
                                              padding));
    }
    if (arrsize == 1) {
        return oneval;
    }
    initconst = LLVMConstStructInContext(gctx->llvmctx, varr, arrsize, 1);
    oneval = LLVMAddGlobal(gctx->module, LLVMTypeOf(initconst), llvmgen_global(gctx));
    LLVMSetLinkage(oneval, LLVMPrivateLinkage);
    LLVMSetInitializer(oneval, initconst);
    free(varr);
    return oneval;

} /* llvmgen_presetter */

/*
 * handle_initializer
 *
 * Handles initialization (INITIAL and PRESET) attributes for a data symbol.
 * Specifics depend on the type of symbol (OWN/GLOBAL/PLIT or LOCAL), and whether
 * we're doing INITIAL or PRESET.
 */
static void
handle_initializer (gencodectx_t gctx, llvm_datasym_t *ld, name_t *np, unsigned int typesize)
{
    unsigned int bpunit = machine_unit_bits(gctx->mach);
    data_attr_t *attr = datasym_attr(np);
    unsigned int nunits, padding;
    LLVMValueRef initval = 0;

    nunits = (unsigned int)initval_size(gctx->symctx, attr->ivlist);
    if (nunits > typesize) {
        if (typesize != 0) {
            log_signal(expr_logctx(gctx->ectx), name_defpos(np), STC__INITSZERR, name_string(np));
        }
        padding = 0;
    } else {
        padding = typesize - nunits;
        if (padding > 1024) {
            expr_signal(gctx->ectx, STC__INTCMPERR, "handle_initializer[1]");
        }
    }
    if (attr->ivlist->preset_expr == 0) {
        initval = llvmgen_initializer(gctx, attr->ivlist, padding, 0);
    } else {
        initval_t *iv;
        for (iv = attr->ivlist; iv != 0; iv = iv->next) {
            expr_node_t *exp = iv->data.scalar.expr;
            if (!expr_is_ltce(exp)) break;
        }
        if (iv == 0) {
            initval = llvmgen_presetter(gctx, attr->ivlist, padding);
        } else {
            llvmgen_memset(gctx, ld->value, LLVMConstNull(LLVMIntTypeInContext(gctx->llvmctx, bpunit)),
                           LLVMConstInt(gctx->fullwordtype, attr->units, 0));
            for (iv = attr->ivlist; iv != 0; iv = iv->next) {
                llvmgen_assignment(gctx, iv->preset_expr, iv->data.scalar.expr);
            }
        }
    }
    if (initval == 0) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "handle_initializer");
        return;
    }
    if (ld->vclass == LLVM_GLOBAL) {
        ld->value = LLVMAddGlobal(gctx->module, LLVMTypeOf(initval), name_azstring(np));
        LLVMSetInitializer(ld->value, initval);
    } else {
        if (typesize <= machine_scalar_bits(gctx->mach)) {
            LLVMValueRef addr = llvmgen_adjustval(gctx, ld->value, LLVMPointerType(LLVMTypeOf(initval), 0), 0);
            LLVMBuildStore(gctx->curfn->builder, initval, addr);
        } else {
            llvmgen_memcpy(gctx, ld->value, initval, LLVMConstInt(gctx->fullwordtype, typesize, 0));
        }
    }

    if (typesize == 0 && attr->units == 0) {
        attr->units = nunits;
    }

} /* handle_initializer */

/*
 * gendatatype
 *
 * Generates the appropriate LLVM type for a data symbol.
 */
static LLVMTypeRef
gendatatype (gencodectx_t gctx, data_attr_t *attr, unsigned int *ucountp)
{
    unsigned int bpunit = machine_unit_bits(gctx->mach);
    unsigned int isref = (attr->flags & SYM_M_REF) != 0;
    LLVMTypeRef basetype;

    if (attr->struc != 0 || attr->units > machine_scalar_units(gctx->mach)) {
        // BINDs cannot be arrays, since they don't allocate storage.
        if (attr->units == 0 || (attr->flags & SYM_M_BIND) != 0) {
            basetype = LLVMIntTypeInContext(gctx->llvmctx, bpunit);
            if (attr->dclass != DCLASS_ARG) {
                basetype = LLVMPointerType(basetype, 0);
            }
        } else {
            basetype = LLVMArrayType(LLVMIntTypeInContext(gctx->llvmctx, bpunit), attr->units);
        }
    } else {
        basetype = LLVMIntTypeInContext(gctx->llvmctx, attr->units * bpunit);
    }
    if (isref) {
        if (ucountp != 0) *ucountp = machine_addr_bits(gctx->mach)/bpunit;
        return LLVMPointerType(basetype, 0);
    }
    if (ucountp != 0) *ucountp = attr->units;
    return basetype;

} /* gendatatype */

/*
 * datasym_generator
 *
 * Generates a data symbol.  Routine arguments and local BINDs are LLVM registers
 * (they don't actually allocate any storage).
 */
static int
datasym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    namectx_t namectx = scope_namectx(name_scope(np));
    llvm_datasym_t *ld = p;
    data_attr_t *attr = datasym_attr(np);
    LLVMTypeRef type;
    unsigned int units;

    if ((attr->flags & SYM_M_FORWARD) != 0) {
        return 1;
    }

    ld->flags = (((attr->flags & SYM_M_SIGNEXT) != 0 ? LLVMGEN_M_SEG_SIGNEXT : 0) |
                 ((attr->flags & SYM_M_VOLATILE) != 0 ? LLVMGEN_M_SEG_VOLATILE : 0) |
                 ((attr->flags & SYM_M_REF) != 0 ? LLVMGEN_M_SEG_ISREF : 0) |
                 ((attr->flags & SYM_M_BIND) != 0 ? LLVMGEN_M_SEG_ISBIND : 0));

    if (attr->dclass == DCLASS_ARG) {
        ld->vclass = LLVM_REG;
    } else if ((attr->flags & SYM_M_BIND) != 0) {
        initval_t *iv = attr->ivlist;
        LLVMValueRef bindval;
        type = gendatatype(gctx, attr, &units);
        if (attr->struc == 0 && attr->units <= machine_scalar_units(gctx->mach)) {
            type = LLVMPointerType(type, 0);
            ld->flags |= LLVMGEN_M_SEG_BINDPTR;
        }
        if (iv->type == IVTYPE_SCALAR) {
            unsigned int bpunit = machine_unit_bits(gctx->mach);
            LLVMValueRef sval;
            sval = LLVMConstInt(LLVMIntTypeInContext(gctx->llvmctx,
                                                     iv->data.scalar.width * bpunit),
                                   iv->data.scalar.value, iv->data.scalar.signext);
            bindval = llvmgen_adjustval(gctx, sval, type,
                                        (attr->flags & SYM_M_SIGNEXT) != 0);
        } else {
            expr_node_t *bindexp = iv->data.scalar.expr;
            bindval = llvmgen_expression(gctx, bindexp, type);
        }
        if (attr->sc == SYMSCOPE_LOCAL) {
            ld->value = bindval;
            LLVMSetValueName(ld->value, name_azstring(np));
            ld->vclass = LLVM_REG;
        } else {
            ld->value = LLVMAddGlobal(gctx->module, type, name_azstring(np));
            LLVMSetInitializer(ld->value, bindval);
            ld->vclass = LLVM_GLOBAL;
            if (name_globalname(namectx, np) == 0) {
                LLVMSetLinkage(ld->value, LLVMInternalLinkage);
            }
        }
    } else if (attr->dclass == DCLASS_STATIC) {
        ld->vclass = LLVM_GLOBAL;
        type = gendatatype(gctx, attr, &units);
        if (attr->ivlist != 0) {
            handle_initializer(gctx, ld, np, units);
        } else {
            ld->value = LLVMAddGlobal(gctx->module, type, name_azstring(np));
            if (attr->sc != SYMSCOPE_EXTERNAL) {
                LLVMSetInitializer(ld->value, LLVMConstNull(type));
            }
        }
        if (attr->owner != 0) {
            LLVMSetSection(ld->value, name_azstring(attr->owner));
        }
        if (name_globalname(namectx, np) == 0) {
            LLVMSetLinkage(ld->value, LLVMInternalLinkage);
        }
        if (attr->alignment != 0) {
            LLVMSetAlignment(ld->value, 1<<attr->alignment);
        }
    } else {
        // Everything else is LOCAL, with an alloca, even REGISTER
        // declarations. XXX revisit later
        ld->vclass = LLVM_LOCAL;
        type = gendatatype(gctx, attr, &units);
        ld->value = LLVMBuildAlloca(gctx->curfn->builder, type, name_azstring(np));
        if (attr->alignment != 0) {
            HelperSetAllocaAlignment(ld->value, 1<<attr->alignment);
        }
        if (attr->ivlist != 0) {
            handle_initializer(gctx, ld, np, units);
        }
    }

    return 1;

} /* datasym_generator */

/*
 * rtnsym_generator
 *
 * Generates a routine symbol.  For EXTERNAL and FORWARD declarations, we
 * generate a generic (varargs) LLVM function prototype.  When a FORWARD declaration
 * is later instantiated by a routine definition, we replace the original
 * prototype with the actual prototype that has the correct number of
 * parameters.
 */
static int
rtnsym_generator (void *vctx, name_t *np, void *p)
{
    gencodectx_t gctx = vctx;
    llvm_rtnsym_t *lr = p;
    routine_attr_t *attr = rtnsym_attr(np);
    nameref_t *ref;
    LLVMValueRef extantfn, arg;
    LLVMTypeRef  argtypes[LLVMGEN_K_MAXARGS];
    int proto_only = ((attr->flags & SYM_M_FORWARD) != 0 || attr->sc == SYMSCOPE_EXTERNAL);
    unsigned int argcount, i;

    extantfn = LLVMGetNamedFunction(gctx->module, name_azstring(np));
    if (extantfn != 0 && proto_only) {
        return 1;
    }
    lr->returntype = ((attr->flags & SYM_M_NOVALUE) == 0 ?
                      LLVMIntTypeInContext(gctx->llvmctx, machine_scalar_bits(gctx->mach)) :
                      LLVMVoidTypeInContext(gctx->llvmctx));
    if (proto_only) {
        lr->type = LLVMFunctionType(lr->returntype, 0, 0, 1);
        lr->func = LLVMAddFunction(gctx->module, name_azstring(np), lr->type);
        return 1;
    }
    argcount = namereflist_length(&attr->inargs);
    if (argcount > LLVMGEN_K_MAXARGS) {
        expr_signal(gctx->ectx, STC__EXCRTNPARS, name_string(np));
        argcount = LLVMGEN_K_MAXARGS;
    }
    for (i = 0, ref = namereflist_head(&attr->inargs); i < argcount; i++, ref = ref->tq_next) {
        argtypes[i] = gendatatype(gctx, datasym_attr(ref->np), 0);
    }
    // XXX need machine-specific VARARG linkage attribute
    lr->type = LLVMFunctionType(lr->returntype, argtypes, argcount, 0);
    if (extantfn != 0) {
        LLVMSetValueName(extantfn, "$$function.for.replacement");
    }
    lr->func = LLVMAddFunction(gctx->module, name_azstring(np), lr->type);
    if (extantfn != 0) {
        LLVMTypeRef extype = LLVMTypeOf(extantfn);
        if (lr->type == extype) {
            LLVMReplaceAllUsesWith(extantfn, lr->func);
        } else {
            LLVMReplaceAllUsesWith(extantfn, LLVMConstBitCast(lr->func, extype));
        }
        LLVMDeleteFunction(extantfn);
    }
    if (name_globalname(expr_namectx(gctx->ectx), np) != 0) {
        LLVMSetLinkage(lr->func, LLVMExternalLinkage);
    } else {
        LLVMSetLinkage(lr->func, LLVMInternalLinkage);
    }
    if (attr->owner != 0) {
        LLVMSetSection(lr->func, name_azstring(attr->owner));
    }
    for (i = 0, ref = namereflist_head(&attr->inargs); i < argcount; i++, ref = ref->tq_next) {
        llvm_datasym_t *ld = sym_genspace(ref->np);
        arg = LLVMGetParam(lr->func, i);
        ld->value = arg;
        LLVMSetValueName(arg, name_azstring(ref->np));
    }

    return 1;

} /* rtnsym_generator */


/*
 * llvmgen_deref_push
 *
 * Pushes notification of the need to automatically dereference a REF
 * symbol.  Called when computing the address for a fetch or an assignment
 * operation.  It's counted because multiple such derefs could appear in
 * a single expression.
 */
void
llvmgen_deref_push (gencodectx_t gctx, name_t *np)
{
    lextype_t ntype = name_type(np);
    llvm_datasym_t *ld;
    if (ntype != LEXTYPE_NAME_DATA) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_deref_push");
    }
    ld = sym_genspace(np);
    ld->deref += 1;

} /* llvmgen_deref_push */

/*
 * llvmgen_deref_pop
 *
 * Pops a dereference notification.
 */
void
llvmgen_deref_pop (gencodectx_t gctx, name_t *np)
{
    lextype_t ntype = name_type(np);
    llvm_datasym_t *ld;
    if (ntype != LEXTYPE_NAME_DATA) {
        expr_signal(gctx->ectx, STC__INTCMPERR, "llvmgen_deref_pop");
    }
    ld = sym_genspace(np);
    ld->deref -= 1;

} /* llvmgen_deref_pop */

/*
 * llvmgen_segaddress
 *
 * Generates an address.  This should only be applied to ROUTINEs
 * and data segments.  Automatically handles auto-dereferencing of REF
 * pointers.
 */
LLVMValueRef
llvmgen_segaddress (gencodectx_t gctx, name_t *np, llvm_stgclass_t *segclassp, unsigned int *flagsp)
{
    lextype_t ntype = name_type(np);

    if (ntype == LEXTYPE_NAME_ROUTINE) {
        llvm_rtnsym_t *lr = sym_genspace(np);
        if (segclassp != 0) *segclassp = LLVM_GLOBAL;
        if (flagsp != 0) *flagsp = machine_addr_signed(gctx->mach) ? LLVMGEN_M_SEG_SIGNEXT : 0;
        return lr->func;
    }
    if (ntype == LEXTYPE_NAME_DATA) {
        llvm_datasym_t *ld = sym_genspace(np);
        LLVMValueRef val;
        if (segclassp != 0) *segclassp = ld->vclass;
        if (flagsp != 0) *flagsp = ld->flags;
        val = ld->value;
        if (ld->deref != 0 && (ld->flags & LLVMGEN_M_SEG_ISREF) != 0) {
            if (flagsp != 0) *flagsp |= LLVMGEN_M_SEG_DEREFED;
            if (ld->vclass != LLVM_REG) {
                val = LLVMBuildLoad(gctx->curfn->builder, val, llvmgen_temp(gctx));
            }
        }
        return val;
    }
    // Should never be called on any other name types
    expr_signal(gctx->ectx, STC__INTCMPERR, "llvm_segaddress");
    return 0;

} /* llvmgen_segaddress */

/*
 * symbol_gen_dispatch
 *
 * Dispatcher routine for generating a symbol.  Called
 * by the front end.
 */
static int
symbol_gen_dispatch (void *vctx, name_t *np, void *p)
{
    switch (name_type(np)) {
        case LEXTYPE_NAME_LITERAL:
            return litsym_generator(vctx, np, p);
        case LEXTYPE_NAME_DATA:
            return datasym_generator(vctx, np, p);
        case LEXTYPE_NAME_ROUTINE:
            return rtnsym_generator(vctx, np, p);
        default:
            return 1;
    }

} /* symbol_gen_dispatch */

/*
 * symbol_gensize
 *
 * Returns the amount of space needed for symbol tracking.
 * Called by the front end when defining a symbol.
 */
static unsigned int
symbol_gensize (void *vctx, lextype_t lt) {
    switch (lt) {
        case LEXTYPE_NAME_DATA:
            return sizeof(llvm_datasym_t);
        case LEXTYPE_NAME_ROUTINE:
            return sizeof(llvm_rtnsym_t);
        case LEXTYPE_NAME_LABEL:
            return sizeof(llvm_label_t);
        case LEXTYPE_NAME_MODULE:
            return sizeof(llvm_module_t);
        case LEXTYPE_NAME_LITERAL:
            return sizeof(llvm_litsym_t);
        default:
            return 0;
    }

} /* symbol_gensize */

/*
 * llvmgen_symgen_init
 *
 * Initializes the code generation size and dispatch
 * hooks invoked from the front-end symbols module.
 */
void
llvmgen_symgen_init (gencodectx_t gctx)
{
    static sym_genvec_t vec = {
        symbol_gensize, symbol_gen_dispatch, 0, 0, 0
    };

    symbols_gen_register(gctx->symctx, gctx, &vec);

} /* llvmgen_symgen_init */
