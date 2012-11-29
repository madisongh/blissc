//
//  storage.c
//  blissc
//
//  Created by Matthew Madison on 11/18/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdlib.h>
#include "machinedef.h"
#include "storage.h"
#include "strings.h"

struct stgctx_s {
    machinedef_t    *mach;
    psect_t         *psects;
    frame_t         *topframe;
    frame_t         *curframe;
    frame_t         *freeframes;
    seg_t           *freesegs;
    initval_t       *freeivs;
};
#define frame_ALLOCOUNT 128
#define SEG_ALLOCOUNT   128
#define IV_ALLOCOUNT    128

#define SEG_M_STACKONLY (1<<15)
#define SEG_M_ALLOCATED (1<<14)
#define SEG_M_USERFLAGS (~(1<<14))

struct psect_s {
    struct psect_s  *next;
    strdesc_t       *name;
    textpos_t        defpos;
    void            *machattr;
    struct seg_s    *segchain, *seglast;
    unsigned long    size;
    unsigned int     attr; // common attributes
};

struct frame_s {
    struct psect_s  *psect;
    struct frame_s  *parent;
    struct seg_s    *segchain, *seglast;
    struct seg_s    *registers[MACH_K_MAXREGS];
    textpos_t        defpos;
    unsigned long    size;
    // XXX will need linkage and register info here
};


struct initval_s {
    struct initval_s *next;
    struct initval_s *lastptr;
    enum { IVTYPE_SCALAR, IVTYPE_STRING, IVTYPE_LIST } type;
    unsigned int repcount;
    union {
        struct {
            long            value;
            unsigned int    width;
            int             signext;
        } scalar;
        strdesc_t           *string;
        struct initval_s    *listptr;
    } data;
};

struct seg_static_s {
    psect_t         *psect;
    unsigned long    offset;
    unsigned long    size;
    initval_t       *initializer, *iv_last;
};
struct seg_external_s {
    strdesc_t       *symbol;
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
struct seg_literal_s {
    unsigned long    value;
    int              has_value;
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
        struct seg_external_s   extinfo;
        struct seg_stack_s   stackinfo;
        struct seg_reg_s     reginfo;
        struct seg_literal_s litinfo;
    }               info;
};

stgctx_t
storage_init (machinedef_t *mach)
{
    struct stgctx_s *ctx = malloc(sizeof(struct stgctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct stgctx_s));
        ctx->mach = mach;
    }
    return ctx;

} /* storage_init */

void
storage_finish (stgctx_t ctx)
{
    return; // XXX to be filled in later

} /* storage_finish */

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
    string_free(psect->name);
    memset(psect, 0xf0, sizeof(psect_t));
    free(psect);
    // XXX - free the frames and segments, too

} /* psect_free */

psect_t *
psect_create (stgctx_t ctx, strdesc_t *name, textpos_t pos, unsigned int attr)
{
    psect_t *psect = psect_alloc(ctx);

    if (psect != 0) {
        psect->name = string_copy(0, name);
        psect->defpos = pos;
        psect->attr = attr;
    }

    return psect;
}

frame_t *
frame_alloc (stgctx_t ctx, textpos_t pos)
{
    frame_t *frm;

    if (ctx->freeframes == 0) {
        int i;
        ctx->freeframes = malloc(sizeof(frame_t)*frame_ALLOCOUNT);
        for (i = 0, frm = ctx->freeframes; i < frame_ALLOCOUNT-1; i++, frm++) {
            frm->parent = frm + 1;
        }
        frm->parent = 0;
    }
    frm = ctx->freeframes;
    ctx->freeframes = frm->parent;
    memset(frm, 0, sizeof(frame_t));
    frm->parent = ctx->curframe;
    frm->defpos = pos;
    ctx->curframe = frm;
    if (ctx->topframe == 0) {
        ctx->topframe = frm;
    }
    return frm;

} /* frame_alloc */

void
frame_free (stgctx_t ctx, frame_t *frm)
{
    ctx->curframe = frm->parent;
    memset(frm, 0xe9, sizeof(frame_t));
    frm->parent = ctx->freeframes;
    ctx->freeframes = frm;

} /* frame_free */

frame_t *
frame_create (stgctx_t ctx, textpos_t pos)
{
    frame_t *frm;

    if (ctx->freeframes == 0) {
        int i;
        ctx->freeframes = malloc(sizeof(frame_t)*frame_ALLOCOUNT);
        for (i = 0, frm = ctx->freeframes; i < frame_ALLOCOUNT-1; i++, frm++) {
            frm->parent = frm + 1;
        }
        frm->parent = 0;
    }
    frm = ctx->freeframes;
    ctx->freeframes = frm->parent;
    memset(frm, 0, sizeof(frame_t));
    frm->parent = ctx->curframe;
    frm->defpos = pos;
    ctx->curframe = frm;
    if (ctx->topframe == 0) {
        ctx->topframe = frm;
    }
    return frm;

} /* frame_alloc */

static seg_t *
seg_alloc (stgctx_t ctx, segtype_t type, textpos_t defpos)
{
    seg_t *seg;

    if (ctx->freesegs == 0) {
        int i;
        ctx->freesegs = malloc(sizeof(seg_t)*SEG_ALLOCOUNT);
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

void
seg_free (stgctx_t ctx, seg_t *seg)
{
    if (seg->flags & SEG_M_ALLOCATED) {
        return; // can't do this once committed to storage
    }
    switch (seg->type) {
        case SEGTYPE_LITERAL:
            break;
        case SEGTYPE_STACK:
            initval_freelist(ctx, seg->info.stackinfo.initializer);
            break;
        case SEGTYPE_STATIC:
            initval_freelist(ctx, seg->info.staticinfo.initializer);
            break;
        case SEGTYPE_EXTERNAL:
            string_free(seg->info.extinfo.symbol);
            break;
        case SEGTYPE_REGISTER:
            initval_freelist(ctx, seg->info.reginfo.initializer);
    }
    memset(seg, 0x7a, sizeof(seg_t));
    seg->next = ctx->freesegs;
    ctx->freesegs = seg;

} /* seg_free */

initval_t *
initval_alloc (stgctx_t ctx)
{
    initval_t *iv;

    if (ctx->freeivs == 0) {
        int i;
        ctx->freeivs = malloc(sizeof(initval_t)*IV_ALLOCOUNT);
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

void
seg_static_psect_set (stgctx_t ctx, seg_t *seg, psect_t *psect)
{
    if (seg->type != SEGTYPE_STATIC) {
        return;
    }
    seg->info.staticinfo.psect = psect;
}

psect_t *
seg_static_psect (stgctx_t ctx, seg_t *seg)
{
    return (seg->type == SEGTYPE_STATIC ?
            seg->info.staticinfo.psect :
            0);
}

frame_t *
seg_stack_frame (stgctx_t ctx, seg_t *seg)
{
    return (seg->type == SEGTYPE_STACK ?
            seg->info.stackinfo.frame :
            (seg->type == SEGTYPE_REGISTER ?
             seg->info.reginfo.frame : 0));
}

strdesc_t *
seg_ext_symbol (stgctx_t ctx, seg_t *seg)
{
    return (seg->type == SEGTYPE_EXTERNAL ?
            seg->info.extinfo.symbol : 0);
}

void
seg_ext_symbol_set (stgctx_t ctx, seg_t *seg, strdesc_t *sym)
{
    if (seg->type != SEGTYPE_EXTERNAL) {
        return;
    }
    seg->info.extinfo.symbol = string_copy(0, sym);
}

unsigned long
seg_size (stgctx_t ctx, seg_t *seg)
{
    if (seg->type == SEGTYPE_STATIC) {
        return seg->info.staticinfo.size;
    } else if (seg->type == SEGTYPE_STACK) {
        return seg->info.stackinfo.size;
    }
    return 0;
}

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

void
initval_freelist (stgctx_t ctx, initval_t *iv)
{
    initval_t *nextiv;

    while (iv != 0) {
        nextiv = iv->next;
        switch (iv->type) {
            case IVTYPE_STRING:
                string_free(iv->data.string);
                break;
            case IVTYPE_LIST:
                initval_freelist(ctx, iv->data.listptr);
            case IVTYPE_SCALAR:
                break;
        }
        iv->next = ctx->freeivs;
        ctx->freeivs = iv;
        iv = nextiv;
    }

} /* initval_freelist */

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
}

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
}

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
    iv->data.string = string_copy(0, str);
    if (listhead == 0) {
        iv->lastptr = iv;
        return iv;
    }
    listhead->lastptr->next = iv;
    listhead->lastptr = iv;
    return listhead;
}

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
}

unsigned long
initval_size (stgctx_t ctx, initval_t *ivlist)
{
    initval_t *iv;
    unsigned long totsize = 0;
    for (iv = ivlist; iv != 0; iv = iv->next) {
        switch (iv->type) {
            case IVTYPE_SCALAR:
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

        default:
            break;
    }
}
static int
seg_initval_add_scalar (stgctx_t ctx, seg_t *seg, unsigned int reps,
                        long value, unsigned int width, int signext)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0 || (seg->type != SEGTYPE_STATIC &&
                    seg->type != SEGTYPE_STACK)) {
        return 0;
    }
    if (reps == 0) {
        return 1;
    }
    iv->repcount = reps;
    iv->type     = IVTYPE_SCALAR;
    iv->data.scalar.width    = width;
    iv->data.scalar.signext  = signext;
    iv->data.scalar.value    = value;
    update_seg(seg, iv);
    return 1;
}

static int
seg_initval_add_string (stgctx_t ctx, seg_t *seg, unsigned int reps,
                        strdesc_t *str)
{
    initval_t *iv = initval_alloc(ctx);

    if (iv == 0 || (seg->type != SEGTYPE_STATIC &&
                    seg->type != SEGTYPE_STACK)) {
        return 0;
    }
    if (reps == 0) {
        return 1;
    }
    iv->repcount = reps;
    iv->type = IVTYPE_STRING;
    iv->data.string = string_copy(0, str);
    update_seg(seg, iv);
    return 1;
}

static int
seg_initval_add_ivlist (stgctx_t ctx, seg_t *seg, unsigned int reps,
                        initval_t *ivlist)
{
    initval_t *iv, *nextiv;

    if (seg->type != SEGTYPE_STATIC && seg->type != SEGTYPE_STACK) {
        return 0;
    }
    while (reps-- > 0) {
        for (iv = ivlist; iv != 0; iv = nextiv) {
            nextiv = iv->next;
            switch (iv->type) {
                case IVTYPE_LIST:
                    seg_initval_add_ivlist(ctx, seg, iv->repcount,
                                           iv->data.listptr);
                    break;
                case IVTYPE_SCALAR:
                    seg_initval_add_scalar(ctx, seg, iv->repcount,
                                           iv->data.scalar.value,
                                           iv->data.scalar.width,
                                           iv->data.scalar.signext);
                    break;
                case IVTYPE_STRING:
                    seg_initval_add_string(ctx, seg, iv->repcount,
                                           iv->data.string);
                    break;
            }
        }
    }
    initval_freelist(ctx, ivlist);
    return 1;
}

int
seg_initval_set (stgctx_t ctx, seg_t *seg, initval_t *ivlist)
{
    unsigned long ivsize = initval_size(ctx, ivlist);
    unsigned long segsize;
    if (seg->type != SEGTYPE_STATIC && seg->type != SEGTYPE_STACK) {
        return 0;
    }
    segsize = seg_size(ctx, seg);

    if (segsize == 0) {
        seg_size_set(ctx, seg, ivsize);
    } else if (ivsize > segsize) {
        return 0;
    }

    return seg_initval_add_ivlist(ctx, seg, 1, ivlist);

} /* seg_initval_set */

int
seg_commit (stgctx_t ctx, seg_t *seg)
{
    if (seg->flags & SEG_M_ALLOCATED) {
        return 0;
    }
    seg_storage_commit(ctx, seg);
    return 1;
}

seg_t *
seg_alloc_static (stgctx_t ctx, textpos_t defpos, psect_t *psect)
{
    seg_t *seg = seg_alloc(ctx, SEGTYPE_STATIC, defpos);

    if (seg == 0) {
        return 0;
    }
    seg->info.staticinfo.psect = psect;    
    return seg;
}

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
}

seg_t *
seg_alloc_register (stgctx_t ctx, textpos_t defpos)
{
    seg_t *seg;

    seg = seg_alloc(ctx, SEGTYPE_REGISTER, defpos);

    if (seg == 0) {
        return 0;
    }
    return seg;
}

seg_t *
seg_alloc_literal (stgctx_t ctx, textpos_t defpos, unsigned long value)
{
    seg_t *seg = seg_alloc(ctx, SEGTYPE_LITERAL, defpos);

    if (seg != 0) {
        seg->type = SEGTYPE_LITERAL;
        seg->info.litinfo.value = value;
    }

    return seg;
}

seg_t *
seg_alloc_external (stgctx_t ctx, textpos_t defpos, strdesc_t *sym)
{
    seg_t *seg = seg_alloc(ctx, SEGTYPE_EXTERNAL, defpos);
    if (seg != 0) {
        seg->type = SEGTYPE_EXTERNAL;
        seg->info.extinfo.symbol = string_copy(0, sym);
    }
    return seg;
}

strdesc_t *
seg_dumpinfo (seg_t *seg)
{
    return 0;
}
