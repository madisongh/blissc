/*
 *++
 *	File:			storage.c
 *
 *	Abstract:		Storage tracking
 *
 *  Module description:
 *		This module manages the information related to storage
 *		for a compiled module: PSECTs, stack frames for routines,
 *		PLITs, and initialization values set via INIT and PRESET
 *		in data declarations.
 *
 *		A storage location is represented by a 'seg_t' structure
 *		('seg' for 'segment').
 *
 *	Author:		M. Madison
 *				Copyright © 2012, Matthew Madison
 *				All rights reserved.
 *	Modification history:
 *		21-Dec-2012	V1.0	Madison		Initial coding.
 *--
 */
#include <stdlib.h>
#include "machinedef.h"
#include "storage.h"
#include "expression.h"
#include "nametable.h"
#include "strings.h"

// Context structure for this module

struct extenthdr_s {
    struct extenthdr_s *next;
};

struct stgctx_s {
    strctx_t         strctx;
    machinedef_t    *mach;
    struct extenthdr_s *extents;
    psect_t         *psects;
    frame_t         *topframe;
    frame_t         *curframe;
    frame_t         *freeframes;
    seg_t           *freesegs;
    initval_t       *freeivs;
};

// Allocation counts for the lookaside lists
// used for the tracking structures
#define FRAME_ALLOCOUNT 128
#define SEG_ALLOCOUNT   128
#define IV_ALLOCOUNT    128

// Internal-use-only flags.
#define SEG_M_STACKONLY (1<<15)
#define SEG_M_ALLOCATED (1<<14)
#define SEG_M_USERFLAGS (~(1<<14))

// PSECT information - for static storage.
struct psect_s {
    struct psect_s  *next;
    strdesc_t       *name;
    textpos_t        defpos;
    void            *machattr;
    struct seg_s    *segchain, *seglast;
    unsigned long    size;
    unsigned int     attr; // common attributes
};

// Frame structure - for stack-allocated storage.
struct frame_s {
    struct frame_s  *parent;
    void            *routine;
    struct seg_s    *segchain, *seglast;
    struct seg_s    *registers[MACH_K_MAXREGS];
    textpos_t        defpos;
    unsigned long    size;
    // XXX will need linkage and register info here
};

// Initval structure - for handling compile-time
// initialization of PLITs and data segments (via INITIAL
// and PRESET).
struct initval_s {
    struct initval_s *next;
    struct initval_s *lastptr;
    enum { IVTYPE_SCALAR, IVTYPE_EXPR_SEG, IVTYPE_EXPR_EXP,
           IVTYPE_STRING, IVTYPE_LIST } type;
    unsigned int repcount;
    void *preset_expr;
    union {
        struct {
            void           *expr;
            long            value;
            unsigned int    width;
            int             signext;
        } scalar;
        strdesc_t           *string;
        struct initval_s    *listptr;
    } data;
};

// A seg_t structure contains a set of common fields
// and a union to hold information specific to a given
// segment type.
// XXX revisit this, once I've figured out how to
//     represent registers, as there is so little difference
//     now between static and stack-allocated segments.
struct seg_static_s {
    psect_t         *psect;
    name_t          *globsym;
    unsigned long    offset;
    unsigned long    size;
    initval_t       *initializer, *iv_last;
    int              is_external;
};
struct seg_stack_s {
    frame_t         *frame;
    unsigned long    offset;
    unsigned long    size;
    initval_t       *initializer, *iv_last;
};
struct seg_reg_s {
    frame_t         *frame;
    unsigned int     regnum;
    unsigned long    size;
    initval_t       *initializer, *iv_last;
    int              regnum_set;
};

struct seg_s {
    struct seg_s    *next;
    textpos_t        defpos;
    void            *machattr;
    segtype_t       type;
    unsigned int    flags;
    unsigned int    alignment;
    union {
        struct seg_static_s  staticinfo;
        struct seg_stack_s   stackinfo;
        struct seg_reg_s     reginfo;
    }               info;
};

/*
 * storage_init
 *
 * Module initialization.
 */
stgctx_t
storage_init (strctx_t strctx, machinedef_t *mach)
{
    struct stgctx_s *ctx = malloc(sizeof(struct stgctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct stgctx_s));
        ctx->mach = mach;
        ctx->strctx = strctx;
    }
    return ctx;

} /* storage_init */

/*
 * storage_finish
 *
 * TBD.  Should free up all of the storage tracking
 * structures.  XXX
 */
void
storage_finish (stgctx_t ctx)
{
    struct extenthdr_s *e, *enext;
    psect_t *p, *pnext;

    if (ctx == 0) {
        return;
    }

    for (p = ctx->psects; p != 0; p = pnext) {
        pnext = p->next;
        string_free(ctx->strctx, p->name);
        free(p);
    }

    for (e = ctx->extents; e != 0; e = enext) {
        enext = e->next;
        free(e);
    }

    free(ctx);

} /* storage_finish */

/*
 * psect_alloc
 *
 * Allocate a psect structure.  Nothing fancy here,
 * since the common case is to have no more than a
 * few psects defined.
 */
static psect_t *
psect_alloc (stgctx_t ctx)
{
    psect_t *psect = malloc(sizeof(psect_t));

    if (psect != 0) {
        memset(psect, 0, sizeof(psect_t));
        psect->next = ctx->psects;
        ctx->psects = psect;
    }
    return psect;
} /* psect_alloc */

/*
 * psect_free
 *
 * Removes a psect from the list and frees it.
 */
static void
psect_free (stgctx_t ctx, psect_t *psect)
{
    psect_t *p, *lp;

    if (psect == 0) {
        return;
    }
    for (p = ctx->psects, lp = 0; p != 0; lp = p, p = p->next) {
        if (p == psect) break;
    }
    if (p != psect) {
        return;
    }
    if (lp == 0) {
        ctx->psects = psect->next;
    } else {
        lp->next = psect->next;
    }
    string_free(ctx->strctx, psect->name);
    memset(psect, 0xf0, sizeof(psect_t));
    free(psect);
    // XXX - free the segments, too?

} /* psect_free */

/*
 * psect_create
 *
 * Public routine for creating a psect.
 */
psect_t *
psect_create (stgctx_t ctx, strdesc_t *name, textpos_t pos, unsigned int attr)
{
    psect_t *psect = psect_alloc(ctx);

    if (psect != 0) {
        psect->name = string_copy(ctx->strctx, 0, name);
        psect->defpos = pos;
        psect->attr = attr;
    }

    return psect;

} /* psect_create */

/*
 * Getters/setters for psect_t
 */
unsigned int psect_flags(psect_t *ps) { return ps->attr; }

/*
 * frame_begin
 *
 * Public routine for creating a stack frame tracking
 * structure (for local storage within a routine).
 *
 * XXX the parent/child relationship used here is
 * incorrect, and not really relevant - such a
 * relationship does not exist at compilation time.
 * Should just use create/free verbs, and track
 * these in a list, like psects.
 */
frame_t *
frame_begin (stgctx_t ctx, textpos_t pos, void *routine)
{
    frame_t *frm;

    if (ctx->freeframes == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s) + sizeof(frame_t)*FRAME_ALLOCOUNT);
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freeframes = (frame_t *)(extent + 1);
        for (i = 0, frm = ctx->freeframes; i < FRAME_ALLOCOUNT-1; i++, frm++) {
            frm->parent = frm + 1;
        }
        frm->parent = 0;
    }
    frm = ctx->freeframes;
    ctx->freeframes = frm->parent;
    memset(frm, 0, sizeof(frame_t));
    frm->parent = ctx->curframe;
    frm->defpos = pos;
    frm->routine = routine;
    ctx->curframe = frm;
    if (ctx->topframe == 0) {
        ctx->topframe = frm;
    }
    return frm;

} /* frame_begin */

/*
 * frame_end
 *
 * Frees a frame-tracking structure.
 */
frame_t *
frame_end (stgctx_t ctx)
{
    frame_t *frm = ctx->curframe;

    if (frm == 0) {
        return 0;
    }
    ctx->curframe = frm->parent;
    memset(frm, 0xe9, sizeof(frame_t));
    frm->parent = ctx->freeframes;
    ctx->freeframes = frm;
    return ctx->curframe;

} /* frame_end */

/*
 * seg_alloc
 *
 * Allocates a segment.
 */
static seg_t *
seg_alloc (stgctx_t ctx, segtype_t type, textpos_t defpos)
{
    seg_t *seg;

    if (ctx->freesegs == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s) + sizeof(seg_t)*SEG_ALLOCOUNT);
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freesegs = (seg_t *)(extent + 1);
        for (i = 0, seg = ctx->freesegs; i < SEG_ALLOCOUNT-1; i++, seg++) {
            seg->next = seg + 1;
        }
        seg->next = 0;
    }
    seg = ctx->freesegs;
    ctx->freesegs = seg->next;
    memset(seg, 0, sizeof(seg_t));
    seg->type = type;
    seg->defpos = defpos;
    return seg;
} /* seg_alloc */

/*
 * seg_free
 *
 * Frees a seg_t and any initvals attached to it.
 */
void
seg_free (stgctx_t ctx, seg_t *seg)
{
    if (seg->flags & SEG_M_ALLOCATED) {
        return; // can't do this once committed to storage
    }
    switch (seg->type) {
        case SEGTYPE_STACK:
            initval_freelist(ctx, seg->info.stackinfo.initializer);
            break;
        case SEGTYPE_STATIC:
            initval_freelist(ctx, seg->info.staticinfo.initializer);
            break;
        case SEGTYPE_REGISTER:
            initval_freelist(ctx, seg->info.reginfo.initializer);
    }
    memset(seg, 0x7a, sizeof(seg_t));
    seg->next = ctx->freesegs;
    ctx->freesegs = seg;

} /* seg_free */

/*
 * initval_alloc
 *
 * Allocate an initval structure.
 */
initval_t *
initval_alloc (stgctx_t ctx)
{
    initval_t *iv;

    if (ctx->freeivs == 0) {
        struct extenthdr_s *extent;
        int i;
        extent = malloc(sizeof(struct extenthdr_s) + sizeof(initval_t)*IV_ALLOCOUNT);
        if (extent == 0) {
            return 0;
        }
        extent->next = ctx->extents;
        ctx->extents = extent;
        ctx->freeivs = (initval_t *)(extent + 1);
        for (i = 0, iv = ctx->freeivs; i < IV_ALLOCOUNT-1; i++, iv++) {
            iv->next = iv + 1;
        }
        iv->next = 0;
    }
    iv = ctx->freeivs;
    ctx->freeivs = iv->next;
    memset(iv, 0, sizeof(initval_t));
    return iv;

} /* initval_alloc */

/*
 * seg_storage_commit
 *
 * Allocate storage for a segment, based on its type
 * and size.
 */
static void
seg_storage_commit (stgctx_t ctx, seg_t *seg)
{
    unsigned long alignadj = (1UL<<seg->alignment)-1;
    if (seg->type == SEGTYPE_STATIC) {
        if (seg->info.staticinfo.size == 0) {
            return;
        }
        if (seg->info.staticinfo.psect != 0) {
            psect_t *psect = seg->info.staticinfo.psect;
            seg->info.staticinfo.offset = (psect->size + alignadj) & ~alignadj;
            psect->size = seg->info.staticinfo.offset + seg->info.staticinfo.size;
            if (psect->segchain == 0) {
                psect->segchain = psect->seglast = seg;
            } else {
                psect->seglast->next = seg;
                psect->seglast = seg;
            }
            seg->flags |= SEG_M_ALLOCATED;
        }
    } else if (seg->type == SEGTYPE_STACK) {
        if (seg->info.stackinfo.size == 0) {
            return;
        }
        seg->info.stackinfo.frame = ctx->curframe;
        if (seg->info.stackinfo.frame != 0) {
            frame_t *frame = seg->info.stackinfo.frame;
            seg->info.stackinfo.offset = (frame->size + alignadj) & ~alignadj;
            frame->size = seg->info.stackinfo.offset + seg->info.stackinfo.size;
            if (frame->segchain == 0) {
                frame->segchain = frame->seglast = seg;
            } else {
                frame->seglast->next = seg;
                frame->seglast = seg;
            }
            seg->flags |= SEG_M_ALLOCATED;
        }
    } else if (seg->type == SEGTYPE_REGISTER) {
        seg->info.reginfo.frame = ctx->curframe;
        if (seg->info.reginfo.frame != 0) {
            frame_t *frame = seg->info.reginfo.frame;
            if (seg->info.reginfo.regnum_set) {
                frame->registers[seg->info.reginfo.regnum] = seg;
            }
        }
        if (seg->info.reginfo.size == 0) {
            seg->info.reginfo.size = machine_scalar_units(ctx->mach);
        }

        // XXX - need to handle registers here

        seg->flags |= SEG_M_ALLOCATED;
    }

} /* seg_storage_commit */

/*
 * Setters/getters for seg_t
 */
void
seg_static_psect_set (stgctx_t ctx, seg_t *seg, psect_t *psect)
{
    if (seg->type != SEGTYPE_STATIC) {
        return;
    }
    seg->info.staticinfo.psect = psect;
}

psect_t *
seg_static_psect (seg_t *seg)
{
    return (seg->type == SEGTYPE_STATIC ?
            seg->info.staticinfo.psect :
            0);
}

frame_t *
seg_stack_frame (seg_t *seg)
{
    return (seg->type == SEGTYPE_STACK ?
            seg->info.stackinfo.frame :
            (seg->type == SEGTYPE_REGISTER ?
             seg->info.reginfo.frame : 0));
}


unsigned long
seg_size (seg_t *seg)
{
    if (seg->type == SEGTYPE_STATIC) {
        return seg->info.staticinfo.size;
    } else if (seg->type == SEGTYPE_STACK) {
        return seg->info.stackinfo.size;
    }
    return 0;
}

/*
 * log2
 *
 * Table for logarithm of powers of 2, use just below
 * for computing alignments.
 */
static int log2(unsigned int n) {
    static int table[] = { -1, 0, 1, -1, 2, -1, -1, -1, 3 };
    if (n >= sizeof(table) || table[n] < 0) return 0;
    return table[n];
}

void
seg_size_set (stgctx_t ctx, seg_t *seg, unsigned long size)
{
    machinedef_t *mach = ctx->mach;

    if (seg->type == SEGTYPE_STATIC) {
        seg->info.staticinfo.size = size;
    } else if (seg->type == SEGTYPE_STACK) {
        seg->info.stackinfo.size = size;
    }

    if (seg->alignment == 0) {
        seg->alignment = (size >  machine_scalar_units(mach) ?
                          log2(machine_scalar_units(mach)) :
                          log2((unsigned int)size));
    }
}

unsigned int
seg_alignment (stgctx_t ctx, seg_t *seg)
{
    return seg->alignment;
}

void
seg_alignment_set (stgctx_t ctx, seg_t *seg, unsigned int a)
{
    seg->alignment = a;
}

unsigned int
seg_flags (stgctx_t ctx, seg_t *seg) {
    return seg->flags & SEG_M_USERFLAGS;
}

void
seg_flags_set (stgctx_t ctx, seg_t *seg, unsigned int flags)
{
    seg->flags = (seg->flags & ~SEG_M_USERFLAGS) |
    (flags & SEG_M_USERFLAGS);
}

segtype_t
seg_type (seg_t *seg) {
    return seg->type;
}

int
seg_register_number_set (stgctx_t ctx, seg_t *seg, unsigned int regnum)
{
    frame_t *frame = ctx->curframe;

    if (seg->flags & SEG_M_ALLOCATED) {
        return 0;
    }
    if (regnum >= machine_register_count(ctx->mach)) {
        return 0;
    }
    if (frame->registers[regnum] != 0) {
        return 0;
    }
    seg->info.reginfo.regnum = regnum;
    seg->info.reginfo.regnum_set = 1;
    return 1;
}

int
seg_has_storage (stgctx_t ctx, seg_t *seg) {
    return (seg->flags & SEG_M_ALLOCATED) != 0;
}

/*
 * initval_freelist
 *
 * Frees a linked list of initval_t structures and
 * their contents.  Reentrant and recursive, as
 * an initval can point to another list of initvals.
 */
void
initval_freelist (stgctx_t ctx, initval_t *iv)
{
    initval_t *nextiv;

    while (iv != 0) {
        nextiv = iv->next;
        switch (iv->type) {
            case IVTYPE_STRING:
                string_free(ctx->strctx, iv->data.string);
                break;
            case IVTYPE_EXPR_EXP:
                break; // XXX possible memory leak here?
            case IVTYPE_LIST:
                initval_freelist(ctx, iv->data.listptr);
            case IVTYPE_SCALAR:
            case IVTYPE_EXPR_SEG:
                break;
        }
        iv->next = ctx->freeivs;
        ctx->freeivs = iv;
        iv = nextiv;
    }

} /* initval_freelist */

/*
 * initval_scalar_add
 *
 * Adds a scalar initialization value to a list.
 */
initval_t *
initval_scalar_add (stgctx_t ctx, initval_t *listhead, unsigned int reps,
                    long val, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = reps;
    iv->data.scalar.expr = 0;
    iv->data.scalar.value = val;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = signext;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_scalar_add */

/*
 * preset_scalar_add
 *
 * Adds a scalar expression to an initval list.
 */
initval_t *
preset_scalar_add (stgctx_t ctx, initval_t *listhead, void *pexp, long val)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = 0;
    iv->preset_expr = pexp;
    iv->data.scalar.expr = 0;
    iv->data.scalar.value = val;
    iv->data.scalar.width = 0;
    iv->data.scalar.signext = 0;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* preset_scalar_add */

/*
 * initval_expr_add
 *
 * Adds an expression initializer to an initval list.
 */
initval_t *
initval_expr_add (stgctx_t ctx, initval_t *listhead, unsigned int reps,
                  int is_expr, void *exp, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = (is_expr ? IVTYPE_EXPR_EXP : IVTYPE_EXPR_SEG);
    iv->repcount = reps;
    iv->data.scalar.expr  = exp;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = signext;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_expr_add */

/*
 * preset_expr_add
 *
 * Adds an expression to an initval list used for PRESET.
 */
initval_t *
preset_expr_add (stgctx_t ctx, initval_t *listhead, void *pexp,
                 int is_expr, void *exp)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = (is_expr ? IVTYPE_EXPR_EXP : IVTYPE_EXPR_SEG);
    iv->repcount = 0;
    iv->preset_expr = pexp;
    iv->data.scalar.expr  = exp;
    iv->data.scalar.width = 0;
    iv->data.scalar.signext = 0;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* preset_expr_add */

/*
 * initval_scalar_prepend
 *
 * Prepends a scalar value to an initval list.   Used
 * for PLIT construction (for the fullword count).
 */
initval_t *
initval_scalar_prepend (stgctx_t ctx, initval_t *listhead, unsigned int reps,
                        long val, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_SCALAR;
    iv->repcount = reps;
    iv->data.scalar.value = val;
    iv->data.scalar.width = width;
    iv->data.scalar.signext = 0;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_scalar_prepend */

/*
 * initval_string_add
 *
 * Adds a literal string initializer.
 */
initval_t *
initval_string_add (stgctx_t ctx, initval_t *listhead, unsigned int reps,
                    strdesc_t *str)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_STRING;
    iv->repcount = reps;
    iv->data.string = string_copy(ctx->strctx, 0, str);
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_string_add */

/*
 * initval_ivlist_add
 *
 * Adds an initval list as a sublist to the current list.
 */
initval_t *
initval_ivlist_add (stgctx_t ctx, initval_t *listhead, unsigned int reps,
                    initval_t *sublist)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0) {
        return 0;
    }
    iv->type = IVTYPE_LIST;
    iv->repcount = reps;
    iv->data.listptr = sublist;
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;

} /* initval_ivlist_add */

/*
 * initval_size
 *
 * Computes the size of an initval list, taking into
 * account repeat counts, sublists, etc.
 */
unsigned long
initval_size (stgctx_t ctx, initval_t *ivlist)
{
    initval_t *iv;
    unsigned long totsize = 0;
    for (iv = ivlist; iv != 0; iv = iv->next) {
        switch (iv->type) {
            case IVTYPE_SCALAR:
            case IVTYPE_EXPR_SEG:
            case IVTYPE_EXPR_EXP:
                totsize += iv->repcount * iv->data.scalar.width;
                break;
            case IVTYPE_STRING:
                totsize += iv->repcount *
                ((iv->data.string->len + machine_unit_maxbytes(ctx->mach)-1) /
                 machine_unit_maxbytes(ctx->mach));
                break;
            case IVTYPE_LIST:
                totsize += iv->repcount * initval_size(ctx, iv->data.listptr);
                break;
        }
    }
    return totsize;

} /* initval_size */

/*
 * update_seg
 *
 * Adds an initval to a segment's initializer list.
 */
static void
update_seg (seg_t *seg, initval_t *iv)
{
    switch (seg->type) {
        case SEGTYPE_STATIC:
            if (seg->info.staticinfo.initializer == 0) {
                seg->info.staticinfo.initializer =
                seg->info.staticinfo.iv_last = iv;
            } else {
                seg->info.staticinfo.iv_last->next = iv;
                seg->info.staticinfo.iv_last = iv;
            }
            break;
        case SEGTYPE_STACK:
            if (seg->info.stackinfo.initializer == 0) {
                seg->info.stackinfo.initializer =
                seg->info.stackinfo.iv_last = iv;
            } else {
                seg->info.stackinfo.iv_last->next = iv;
                seg->info.stackinfo.iv_last = iv;
            }
            break;
        case SEGTYPE_REGISTER:
            if (seg->info.reginfo.initializer == 0) {
                seg->info.reginfo.initializer =
                seg->info.reginfo.iv_last = iv;
            } else {
                seg->info.reginfo.iv_last->next = iv;
                seg->info.reginfo.iv_last = iv;
            }
            break;

        default:
            break;
    }

} /* update_seg */

/*
 * seg_initval_set
 *
 * Sets a segment's initializer.  If the segment hasn't
 * been allocated yet, allocates space to hold the initial
 * values.  If the segment does have an allocation, validates
 * that the initializer will fit.
 */
int
seg_initval_set (stgctx_t ctx, seg_t *seg, initval_t *ivlist)
{
    unsigned long ivsize = initval_size(ctx, ivlist);
    unsigned long segsize;
    if (seg->type != SEGTYPE_STATIC && seg->type != SEGTYPE_STACK) {
        return 0;
    }
    segsize = seg_size(seg);

    if (segsize == 0) {
        seg_size_set(ctx, seg, ivsize);
    } else if (ivsize > segsize) {
        return 0;
    }

	update_seg(seg, ivlist);
    return 1;

} /* seg_initval_set */

/*
 * seg_preset_set
 *
 * Sets a segment PRESET-style initializer.  No checks here,
 * since we would have to check each offset, and that's too
 * much work. XXX
 */
int
seg_preset_set (stgctx_t ctx, seg_t *seg, initval_t *ivlist)
{
    update_seg(seg, ivlist);
    return 1;

} /* seg_preset_set */

/*
 * seg_commit
 *
 * Public API for committing a segment to storage.
 * Checks to make sure it hasn't already been allocated.
 */
int
seg_commit (stgctx_t ctx, seg_t *seg)
{
    if (seg->flags & SEG_M_ALLOCATED) {
        return 0;
    }
    seg_storage_commit(ctx, seg);
    return 1;

} /* seg_commit */

/*
 * seg_alloc_static
 *
 * Allocates a static seg, assigning it to a psect.
 */
seg_t *
seg_alloc_static (stgctx_t ctx, textpos_t defpos, psect_t *psect)
{
    seg_t *seg;

    seg = seg_alloc(ctx, SEGTYPE_STATIC, defpos);
    if (seg == 0) {
        return 0;
    }
    seg->info.staticinfo.psect = psect;
    seg->info.staticinfo.is_external = 0;
    return seg;

} /* seg_alloc_static */

/*
 * seg_alloc_stack
 *
 * Allocates a stack-local segment.  Caller can specify
 * stackonly to force it to be stack-allocated; otherwise,
 * the back end is free to locate the segment in a register
 * during optimization.
 */
seg_t *
seg_alloc_stack (stgctx_t ctx, textpos_t defpos, int stackonly)
{
    seg_t *seg;

    seg = seg_alloc(ctx, SEGTYPE_STACK, defpos);

    if (seg == 0) {
        return 0;
    }
    if (stackonly) {
        seg->flags |= SEG_M_STACKONLY;
    }
    return seg;

} /* seg_alloc_stack */

/*
 * seg_alloc_register
 *
 * XXX To be filled in
 */
seg_t *
seg_alloc_register (stgctx_t ctx, textpos_t defpos)
{
    seg_t *seg;

    seg = seg_alloc(ctx, SEGTYPE_REGISTER, defpos);

    if (seg == 0) {
        return 0;
    }
    return seg;

} /* seg_alloc_register */

/*
 * seg_addr_is_ltce
 *
 * Returns 1 if the segment's address is a link-time
 * constant (i.e., is static storage); otherwise,
 * returns zero.
 */
int
seg_addr_is_ltce (seg_t *seg)
{
    return (seg_type(seg) == SEGTYPE_STATIC);

} /* seg_addr_is_ltce */

