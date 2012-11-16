//
//  lexer.c
//  blissc
//
//  Low-level lexeme handling.  Maintains the stream of lexemes
//  used by the parser; lexemes can be inserted into the stream,
//  files can be passed to the scanner routines for reading and
//  tokenizing.
//
//  At this level, only the most basic token-to-lexeme conversion
//  happens -- specifically, conversion of operators and delimiters,
//  and the primitive numeric and string literals.
//
//  This module also has the upper layer of lexeme memory management,
//  with knowledge about the 'data' portion of lexeme types that
//  need to have memory freed.    XXX - may need to revisit this
//
//  Created by Matthew Madison on 10/23/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "lexeme.h"
#include "lexer.h"
#include "scanner.h"
#include "strings.h"

struct saved_filename_s {
    struct saved_filename_s *next;
    strdesc_t *filename;
    int filename_index;
};
static struct saved_filename_s *saved_filenames;
static int filename_count;

/*
 * This structure is for inserting a new chain of lexemes
 * into the parsing stream.  When lexical processing adds
 * or replaces lexemes in the stream, they are prepended to
 * the linked list pointed to by 'head'.  When files are
 * opened (e.g., REQUIRE), the scanner context is maintained
 * in 'sctx'.
 */
struct lexchain_s {
    struct lexchain_s   *nextchain;
    lexseq_t             seq;
    scanctx_t            sctx;
    int                  filename_index;
};
typedef struct lexchain_s lexchain_t;

/*
 * Context for this module.  Contains the linked list of
 * lexchain_t structures that represent the parsing stream.
 * When the end of the stream is reached, 'atend' is set;
 * when set, lexer_next() simply returns LEXTYPE_END as
 * the next lexeme, no matter how many times it is called.
 * This eliminates the need for a bunch of code in the
 * layers above to make sure we don't lose the end-of-stream
 * marker.
 */
struct lexer_ctx_s {
    lexchain_t   *chain;
    int          atend;
    int          signok;
};

/*
 * Maps of characters to their respective operator and
 * delimiter lexeme types.
 */
static const char operators[] = "+-*/=.^";
static lextype_t opertypes[] = {
    LEXTYPE_OP_ADD, LEXTYPE_OP_SUB, LEXTYPE_OP_MUL,
    LEXTYPE_OP_DIV, LEXTYPE_OP_ASSIGN, LEXTYPE_OP_FETCH,
    LEXTYPE_OP_SHIFT
};

static const char delimiters[] = ",;:()[]<>%";
static lextype_t delimtypes[] = {
    LEXTYPE_DELIM_COMMA, LEXTYPE_DELIM_SEMI, LEXTYPE_DELIM_COLON,
    LEXTYPE_DELIM_LPAR, LEXTYPE_DELIM_RPAR,
    LEXTYPE_DELIM_LBRACK, LEXTYPE_DELIM_RBRACK,
    LEXTYPE_DELIM_LANGLE, LEXTYPE_DELIM_RANGLE,
    LEXTYPE_LXF_DELIM_PERCENT
};

/*
 * These static lexemes are used to simplify the
 * logic in the parser.
 */
static lexeme_t errlex = { 0, LEXTYPE_NONE, LEXTYPE_NONE };
static lexeme_t endlex = { 0, LEXTYPE_END, LEXTYPE_END };

/*
 * Operators that are keywords.  These are common enough to put down
 * at this level. XXX - revisit
 */
#define DODEFS \
DODEF(AND) DODEF(EQV) DODEF(OR) \
DODEF(NOT) DODEF(XOR) \
DODEF(EQL) DODEF(EQLU) DODEF(EQLA) \
DODEF(GEQ) DODEF(GEQU) DODEF(GEQA) \
DODEF(GTR) DODEF(GTRU) DODEF(GTRA) \
DODEF(LSS) DODEF(LSSU) DODEF(LSSA) \
DODEF(LEQ) DODEF(LEQU) DODEF(LEQA) \
DODEF(NEQ) DODEF(NEQU) DODEF(NEQA)
#define DODEF(name_) NAMEDEF(#name_, LEXTYPE_OP_##name_, NAME_M_RESERVED),

static name_t operator_names[] = {
    DODEFS
};
#undef DODEF
#undef DODEFS


/*
 * -- lexchain memory management --
 */

/*
 * lexchain_alloc
 *
 * Allocate a new lexchain_t.
 */
static lexchain_t *
lexchain_alloc (void)
{
    lexchain_t *chain = malloc(sizeof(struct lexchain_s));
    if (chain != 0) {
        memset(chain, 0, sizeof(struct lexchain_s));
        lexseq_init(&chain->seq);
    }
    return chain;

} /* lexchain_alloc */

/*
 * lexchain_free
 *
 * Frees a lexchain_t, along with all of the lexemes on
 * its linked list, and closing the scanner context.
 */
static void
lexchain_free (lexchain_t *chain)
{
    if (chain != 0) {
        lexseq_free(&chain->seq);
        if (chain->sctx != 0) {
            scan_finish(chain->sctx);
        }
        free(chain);
    }
} /* lexchain_free */

/*
 * Filename tracking for lexeme position records
 */
static int
filename_lookup (const char *name, size_t len, strdesc_t **fdsc) {

    struct saved_filename_s *sf, *lastsf;
    strdesc_t namedsc;

    strdesc_init(&namedsc, (char *)name, len);
    for (sf = saved_filenames, lastsf = 0; sf != 0; lastsf = sf, sf = sf->next) {
        if (strings_eql(sf->filename, &namedsc)) {
            break;
        }
    }

    if (sf == 0) {
        sf = malloc(sizeof(struct saved_filename_s));
        if (lastsf == 0) {
            saved_filenames = sf;
        } else {
            lastsf->next = sf;
        }
        sf->next = 0;
        sf->filename = string_from_chrs(0, name, len);
        sf->filename_index = filename_count;
        filename_count += 1;
    }

    if (fdsc != 0) {
        *fdsc = sf->filename;
    }
    return sf->filename_index;
} /* filename_lookup */

/* --- Public API --- */

/*
 * lexer_init
 *
 * One-time lexer initialization, to register the operator names.
 */
void
lexer_init (scopectx_t kwdscope)
{
    int i;
    for (i = 0; i < sizeof(operator_names)/sizeof(operator_names[0]); i++) {
        name_insert(kwdscope, &operator_names[i]);
    }
    filename_count = 0;
    saved_filenames = 0;
    
} /* lexer_init */

/*
 * lexer_filename
 *
 * Given a filename index (from a lexeme's position),
 * return the relevant filename.
 */
strdesc_t *
lexer_filename (int filename_index)
{
    struct saved_filename_s *sf;
    for (sf = saved_filenames; sf != 0; sf = sf->next) {
        if (sf->filename_index == filename_index) {
            return sf->filename;
        }
    }
    return 0;

} /* filename_by_index */

/*
 * lexer_fopen
 *
 * Called to begin scanning for lexemes in a file.
 */
lexer_ctx_t
lexer_fopen (const char *fname, size_t fnlen)
{
    lexer_ctx_t ctx = malloc(sizeof(struct lexer_ctx_s));
    if (ctx != 0) {
        
        memset(ctx, 0, sizeof(struct lexer_ctx_s));
        ctx->chain = lexchain_alloc();
        ctx->chain->sctx = scan_init();
        if (ctx->chain->sctx == 0) {
            lexchain_free(ctx->chain);
            free(ctx);
            ctx = 0;
        } else {
            if (!scan_fopen(ctx->chain->sctx,
                           fname, fnlen)) {
                lexchain_free(ctx->chain);
                free(ctx);
                ctx = 0;
            } else {
                ctx->chain->filename_index =
                    filename_lookup(fname, fnlen, 0);
            }
        }
    }
    if (ctx != 0) {
        ctx->signok = 1;
    }
    return ctx;

} /* lexer_fopen */

/*
 * lexer_finish
 *
 * Shuts down the lexer, freeing up all of the lexchains.
 */
void
lexer_finish (lexer_ctx_t ctx)
{
    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        ctx->chain = chain->nextchain;
        lexchain_free(chain);
    }
    free(ctx);

} /* lexer_finish */

/*
 * lexer_next
 *
 * Returns the next lexeme in the stream.
 * If 'erroneof' is set, reaching the end of a file (not the
 * end of the stream) should be considered an error condition --
 * this happens when we're in the middle of parsing a lexical
 * conditional or a macro definition; they must be wholly contained
 * within a single file.
 */
static lexeme_t *
lexer___next (lexer_ctx_t ctx, int erroneof, int peek)
{
    lexeme_t *lex = 0;
    lextype_t lextype;
    strdesc_t *tok;
    unsigned int lineno, column;
    char *cp;

    if (ctx == 0) {
        return &errlex;
    }

    if (ctx->atend) {
        return &endlex;
    }

    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        lex = (peek ? lexseq_head(&chain->seq) :lexseq_remhead(&chain->seq));
        if (lex != 0) {
            break;
        }
        if (chain->sctx != 0) {
            scantype_t type;
            unsigned int sflags;

            sflags = (erroneof ? SCAN_M_ERRONEOF : 0) |
                     (ctx->signok ? SCAN_M_SIGNOK : 0);

            type = scan_getnext(chain->sctx, sflags, &tok,
                                &lineno, &column);
            if (!scan_ok(type)) {
                /* XXX error condition */
                lex = &errlex;
                string_free(tok);
                break;
            }
            if (type == SCANTYPE_END) {
                scan_finish(chain->sctx);
                chain->sctx = 0;
                ctx->chain = chain->nextchain;
                lexchain_free(chain);
                if (ctx->chain == 0) {
                    lex = lexeme_alloc(LEXTYPE_END, 0, 0);
                    ctx->atend = 1;
                    string_free(tok);
                    break;
                }
                string_free(tok);
                continue;
            }
            switch (type) {
                case SCANTYPE_DECLITERAL:
                    lextype = LEXTYPE_NUMERIC;
                    ctx->signok = 0;
                    break;
                case SCANTYPE_QUOTEDSTRING:
                    lextype = LEXTYPE_STRING;
                    ctx->signok = 1;
                    break;
                case SCANTYPE_OPERATOR:
                    cp = strchr(operators, *tok->ptr);
                    lextype = opertypes[cp-operators];
                    ctx->signok = 1;
                    break;
                case SCANTYPE_PUNCTUATION:
                    cp = strchr(delimiters, *tok->ptr);
                    lextype = delimtypes[cp-delimiters];
                    ctx->signok = (strchr(")>]", *tok->ptr) != 0);
                    break;
                case SCANTYPE_IDENTIFIER:
                    lextype = LEXTYPE_NAME;
                    ctx->signok = 0;
                    break;
                default:
                    break;
            }
            if (lextype == 0) {
                lex = &errlex;
            } else {
                lex = lexeme_create(lextype, tok);
                lexeme_setpos(lex, chain->filename_index, lineno, column);
                if (peek) {
                    lexseq_inshead(&chain->seq, lex);
                }
            }
            string_free(tok);
            break;
        } else {
            ctx->chain = chain->nextchain;
            lexchain_free(chain);
            if (ctx->chain == 0) {
                lex = lexeme_alloc(LEXTYPE_END, 0, 0);
                ctx->atend = 1;
                break;
            }
        }
    }
    return lex;

} /* lexer___next */

lexeme_t *lexer_next (lexer_ctx_t ctx, int erroneof) {
    return lexer___next(ctx, erroneof, 0);
}

lexeme_t *lexer_peek (lexer_ctx_t ctx, int erroneof) {
    return lexer___next(ctx, erroneof, 1);
}

/*
 * lexer_insert
 *
 * Insert a single lexeme to the front of the stream.
 */
void
lexer_insert (lexer_ctx_t ctx, lexeme_t *lex)
{
    lexchain_t *chain = ctx->chain;

    // The static ones should never be inserted, and
    // just ignore nulls.
    if (lex == &errlex || lex == &endlex || lex == 0) {
        return;
    }
    
    if (chain == 0) {
        chain = lexchain_alloc();
        if (chain == 0) {
            /* XXX error condition */
            return;
        }
        chain->nextchain = ctx->chain;
        ctx->chain = chain;
    }
    lexseq_inshead(&chain->seq, lex);

} /* lexer_insert */

void
lexer_insert_seq (lexer_ctx_t ctx, lexseq_t *seq)
{
    lexchain_t *chain = ctx->chain;


    if (seq == 0 || lexseq_length(seq) == 0) {
        return;
    }
    if (chain == 0) {
        chain = lexchain_alloc();
        if (chain == 0) {
            /* XXX error condition */
            return;
        }
        chain->nextchain = ctx->chain;
        ctx->chain = chain;
    }
    lexseq_prepend(&chain->seq, seq);
}

/*
 * lexer_newfile
 * Insert a new file at the front of the stream (for REQUIRE and %REQUIRE).
 */
int
lexer_newfile (lexer_ctx_t ctx, const char *fname, size_t fnlen)
{
    lexchain_t *chain = lexchain_alloc();

    if (chain == 0) {
        /* XXX error condition */
        return 0;
    }
    chain->sctx = scan_init();
    if (chain->sctx == 0) {
        lexchain_free(chain);
        return 0;
    }
    if (!scan_fopen(chain->sctx, fname, fnlen)) {
        scan_finish(chain->sctx);
        chain->sctx = 0;
        lexchain_free(chain);
        return 0;
    }
    chain->nextchain = ctx->chain;
    ctx->chain = chain;
    return 1;

} /* lexer_newfile */