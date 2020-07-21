/*
 *++
 * lexer.c - Low-level lexical processing.
 *
 * Low-level lexeme handling.  Maintains the stream of lexemes
 * used by the parser; lexemes can be inserted into the stream,
 * files can be passed to the scanner routines for reading and
 * tokenizing.
 *
 * At this level, only the most basic token-to-lexeme conversion
 * happens -- specifically, conversion of operators and delimiters,
 * and the primitive numeric and string literals.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "blissc/lexeme.h"
#include "blissc/lexer.h"
#include "scanner.h"
#include "blissc/support/logging.h"
#include "blissc/support/strings.h"

struct saved_filename_s {
    struct saved_filename_s *next;
    strdesc_t *filename;
    int filename_index;
};

/*
 * This structure is for inserting a new chain of lexemes
 * into the parsing stream.  When lexical processing adds
 * or replaces lexemes in the stream, they are prepended to
 * the linked list pointed to by 'head'.  When files are
 * opened (e.g., REQUIRE), the scanner stream context is
 * maintained in 'strm'.
 */
struct lexchain_s {
    struct lexchain_s   *nextchain;
    lexseq_t             seq;
    streamctx_t          strm;
    int                  filename_index;
    unsigned int         curline;
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
    logctx_t                 logctx;
    strctx_t                 strctx;
    lexctx_t                 lexctx;
    void                    *fioctx;
    scanctx_t                scnctx;
    struct saved_filename_s *saved_filenames;
    lexchain_t              *chain;
    lexchain_t              *freechains;
    int                      filename_count;
    int                      atend;
    int                      signok;
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
static lexeme_t errlex = { .type = LEXTYPE_NONE, .boundtype = LEXTYPE_NONE };
static lexeme_t endlex = { .type = LEXTYPE_END, .boundtype = LEXTYPE_END };

/*
 * Operators that are keywords.  These are common enough to put down
 * at this level.
 */
#define DODEFS \
DODEF(AND) DODEF(EQV) DODEF(OR) \
DODEF(NOT) DODEF(XOR) DODEF(MOD) \
DODEF(EQL) DODEF(EQLU) DODEF(EQLA) \
DODEF(GEQ) DODEF(GEQU) DODEF(GEQA) \
DODEF(GTR) DODEF(GTRU) DODEF(GTRA) \
DODEF(LSS) DODEF(LSSU) DODEF(LSSA) \
DODEF(LEQ) DODEF(LEQU) DODEF(LEQA) \
DODEF(NEQ) DODEF(NEQU) DODEF(NEQA)
#define DODEF(name_) NAMEDEF(#name_, LEXTYPE_OP_##name_, NAME_M_RESERVED),

static namedef_t operator_names[] = {
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
lexchain_alloc (lexer_ctx_t ctx)
{
    lexchain_t *chain;
    if (ctx->freechains == 0) {
        chain = malloc(sizeof(struct lexchain_s));
    } else {
        chain = ctx->freechains;
        ctx->freechains = chain->nextchain;
    }
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
 * its linked list.  Scanner stream should be closed prior
 * to calling this.
 */
static void
lexchain_free (lexer_ctx_t ctx, lexchain_t *chain)
{
    if (chain != 0) {
        lexseq_free(ctx->lexctx, &chain->seq);
        chain->nextchain = ctx->freechains;
        ctx->freechains = chain;
    }

} /* lexchain_free */

/*
 * Filename tracking for lexeme position records
 */
static int
filename_lookup (lexer_ctx_t lctx, const char *name, size_t len,
                 strdesc_t **fdsc) {

    struct saved_filename_s *sf, *lastsf;
    strdesc_t namedsc;

    strdesc_init(&namedsc, (char *)name, len);
    for (sf = lctx->saved_filenames, lastsf = 0; sf != 0;
         lastsf = sf, sf = sf->next) {
        if (strings_eql(sf->filename, &namedsc)) {
            break;
        }
    }

    if (sf == 0) {
        sf = malloc(sizeof(struct saved_filename_s));
        if (lastsf == 0) {
            lctx->saved_filenames = sf;
        } else {
            lastsf->next = sf;
        }
        sf->next = 0;
        sf->filename = string_from_chrs(lctx->strctx, 0, name, len);
        sf->filename_index = lctx->filename_count;
        lctx->filename_count += 1;
    }

    if (fdsc != 0) {
        *fdsc = sf->filename;
    }
    return sf->filename_index;

} /* filename_lookup */

/*
 * lexer_filename
 *
 * Given a filename index (from a lexeme's position),
 * return the relevant filename.  This is invoked through
 * the 'filename_fetch' function pointer by the logger.
 */
static strdesc_t *
lexer_filename (void *vctx, int filename_index)
{
    lexer_ctx_t lctx = vctx;
    struct saved_filename_s *sf;
    static strdesc_t internalsrc = STRDEF("<Internal source>");

    if (filename_index == -1) {
        return &internalsrc;
    }

    for (sf = lctx->saved_filenames; sf != 0; sf = sf->next) {
        if (sf->filename_index == filename_index) {
            return sf->filename;
        }
    }
    return 0;

} /* lexer_filename */

/*
 * lexer_getline
 *
 * If the specified filename index and line number match the current
 * ones, calls the scanner to return the current line.
 * Invoked via the logger's line-fetch hook.
 */
static strdesc_t *
lexer_getline (void *vctx, int fileno, unsigned int lineno)
{
    lexer_ctx_t lctx = vctx;
    lexchain_t *chain = lctx->chain;

    if (chain == 0 || chain->strm == 0) return 0;
    if (fileno == chain->filename_index && chain->curline == lineno) {
        return scan_curline_get(chain->strm);
    }
    return 0;

} /* lexer_getline */


/* --- Public API --- */

/*
 * lexer_init
 *
 * One-time lexer initialization, to register the operator names and
 * the filename fetcher for the logger.  The lexeme module initialization
 * is performed here as well.
 */
lexer_ctx_t
lexer_init (strctx_t strctx, scopectx_t kwdscope, logctx_t logctx, void *fioctx)
{
    lexer_ctx_t ctx;
    unsigned int i;

    for (i = 0; i < sizeof(operator_names)/sizeof(operator_names[0]); i++) {
        name_declare(kwdscope, &operator_names[i], 0, 0, 0, 0);
    }

    ctx = malloc(sizeof(struct lexer_ctx_s));
    if (ctx != 0) {
        memset(ctx, 0, sizeof(struct lexer_ctx_s));
        ctx->scnctx = scan_init(strctx, logctx, fioctx);
        if (ctx->scnctx == 0) {
            free(ctx);
            return 0;
        }
        ctx->strctx = strctx;
        ctx->logctx = logctx;
        log_fetchfn_set(logctx, lexer_filename, ctx);
        log_linefetchfn_set(logctx, lexer_getline, ctx);
        ctx->signok = 1;
        ctx->lexctx = lexeme_init(strctx, logctx);
    }

    return ctx;

} /* lexer_init */

/*
 * Getter/setters for lexer context
 */
lexctx_t lexer_lexctx(lexer_ctx_t lctx) { return lctx->lexctx; }
void *lexer_scanctx(lexer_ctx_t lctx) { return lctx->scnctx; }

/*
 * lexer_fopen
 * Insert a new file at the front of the stream.
 */
int
lexer_fopen (lexer_ctx_t ctx, const char *fname, size_t fnlen,
             char **actnamep)
{
    lexchain_t *chain = lexchain_alloc(ctx);
    char *actname;

    if (chain == 0) {
        log_signal(ctx->logctx, 0, STC__OUTOFMEM, "lexer_fopen");
        return 0;
    }
    chain->strm = scan_fopen(ctx->scnctx, fname, fnlen, &actname);
    if (chain->strm == 0) {
        lexchain_free(ctx, chain);
        return 0;
    }
    chain->filename_index = filename_lookup(ctx, actname, strlen(actname), 0);
    chain->nextchain = ctx->chain;
    ctx->chain = chain;
    if (actnamep != 0) *actnamep = actname;
    ctx->atend = 0;
    return 1;

} /* lexer_fopen */

/*
 * lexer_popen
 * Insert a programmatic input source at the front of the stream.
 */
int
lexer_popen (lexer_ctx_t ctx, void *vinfn, void *fnctx)
{
    scan_input_fn infn = vinfn;
    lexchain_t *chain = lexchain_alloc(ctx);

    if (chain == 0) {
        log_signal(ctx->logctx, 0, STC__OUTOFMEM, "lexer_popen");
        return 0;
    }
    chain->strm = scan_popen(ctx->scnctx, infn, fnctx);
    if (chain->strm == 0) {
        lexchain_free(ctx, chain);
        return 0;
    }
    chain->filename_index = -1;
    chain->nextchain = ctx->chain;
    ctx->chain = chain;
    ctx->atend = 0;
    return 1;

} /* lexer_popen */

/*
 * lexer_finish
 *
 * Shuts down the lexer.
 */
void
lexer_finish (lexer_ctx_t ctx)
{
    lexchain_t *chain, *nextchain;
    struct saved_filename_s *sf, *sfnext;

    for (chain = ctx->chain; chain != 0; chain = nextchain) {
        nextchain = chain->nextchain;
        scan_close(chain->strm);
        lexchain_free(ctx, chain);
    }
    for (chain = ctx->freechains; chain != 0; chain = nextchain) {
        nextchain = chain->nextchain;
        free(chain);
    }
    for (sf = ctx->saved_filenames; sf != 0; sf = sfnext) {
        sfnext = sf->next;
        free(sf);
    }
    lexeme_finish(ctx->lexctx);
    scan_finish(ctx->scnctx);
    free(ctx);

} /* lexer_finish */

/*
 * lexer___next
 *
 * Returns the next lexeme in the stream.
 * If 'erroneof' is set, reaching the end of a file (not the
 * end of the stream) should be considered an error condition --
 * this happens when we're in the middle of parsing a lexical
 * conditional or a macro definition; they must be wholly contained
 * within a single file.
 *
 * This is the internal version that handles both lexer_next
 * and lexer_peek processing.
 */
static lexeme_t *
lexer___next (lexer_ctx_t ctx, int erroneof, int peek, textpos_t *posp)
{
    lexeme_t *lex = 0;
    lextype_t lextype;
    strdesc_t *tok;
    unsigned int column;
    char *cp;

    if (ctx == 0) {
        return &errlex;
    }

    if (ctx->atend) {
        return &endlex;
    }

    while (ctx->chain != 0) {
        lexchain_t *chain = ctx->chain;
        lex = (peek ? lexseq_head(&chain->seq) : lexseq_remhead(&chain->seq));
        if (lex != 0) {
            break;
        }
        if (chain->strm != 0) {
            scantype_t type;
            unsigned int sflags;

            sflags = (erroneof ? SCAN_M_ERRONEOF : 0) |
                     (ctx->signok ? SCAN_M_SIGNOK : 0);

            type = scan_getnext(chain->strm, sflags, &tok,
                                &chain->curline, &column);
            if (!scan_ok(type)) {
                textpos_t pos = textpos_create(chain->filename_index,
                                               chain->curline, column);
                log_signal(ctx->logctx, pos, STC__INVTOKEN);
                lex = &errlex;
                string_free(ctx->strctx, tok);
                break;
            }
            if (!peek && posp != 0) {
                *posp = textpos_create(chain->filename_index, chain->curline, column);
            }
            if (type == SCANTYPE_END) {
                scan_close(chain->strm);
                chain->strm = 0;
                ctx->chain = chain->nextchain;
                lexchain_free(ctx, chain);
                if (ctx->chain == 0) {
                    lex = &endlex;
                    ctx->atend = 1;
                    string_free(ctx->strctx, tok);
                    break;
                }
                string_free(ctx->strctx, tok);
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
                    ctx->signok = (strchr(")>]", *tok->ptr) == 0);
                    break;
                case SCANTYPE_IDENTIFIER:
                    lextype = LEXTYPE_NAME;
                    ctx->signok = 0;
                    break;
                default:
                    lextype = LEXTYPE_NONE;
                    break;
            }
            if (lextype == LEXTYPE_NONE) {
                lex = &errlex;
            } else {
                lex = lexeme_create(ctx->lexctx, lextype, tok);
                if (peek) {
                    lexseq_inshead(&chain->seq, lex);
                }
            }
            string_free(ctx->strctx, tok);
            break;
        } else {
            ctx->chain = chain->nextchain;
            lexchain_free(ctx, chain);
            if (ctx->chain == 0) {
                lex = &endlex;
                ctx->atend = 1;
                break;
            }
        }
    }
    return lex;

} /* lexer___next */

/*
 * lexer_next
 *
 * Public API for fetching the next lexeme from the
 * input stream.
 */
lexeme_t *lexer_next (lexer_ctx_t ctx, int erroneof, textpos_t *posp)
{
    return lexer___next(ctx, erroneof, 0, posp);

} /* lexer_next */

/*
 * lexer_peek
 *
 * Public API for peeking at the next lexeme in the
 * input stream, without actually removing it.
 */
lexeme_t *lexer_peek (lexer_ctx_t ctx, int erroneof)
{
    return lexer___next(ctx, erroneof, 1, 0);

} /* lexer_peek */

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
        chain = lexchain_alloc(ctx);
        if (chain == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "lexer_insert");
            return;
        }
        chain->nextchain = ctx->chain;
        ctx->chain = chain;
    }
    lexseq_inshead(&chain->seq, lex);

} /* lexer_insert */

/*
 * lexer_insert_seq
 *
 * Inserts a sequence of lexemes at the front of the stream.
 */
void
lexer_insert_seq (lexer_ctx_t ctx, lexseq_t *seq)
{
    lexchain_t *chain = ctx->chain;

    if (seq == 0 || lexseq_length(seq) == 0) {
        return;
    }
    if (chain == 0) {
        chain = lexchain_alloc(ctx);
        if (chain == 0) {
            log_signal(ctx->logctx, 0, STC__OUTOFMEM, "lexer_insert_seq");
            return;
        }
        chain->nextchain = ctx->chain;
        ctx->chain = chain;
    }
    lexseq_prepend(&chain->seq, seq);

} /* lexer_insert_seq_internal */
