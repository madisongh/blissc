#ifndef parser_h__
#define parser_h__
/*
 *++
 * parser.h - Parser definitions.
 *
 * Copyright © 2012, Matthew Madison.
 * All rights reserved.
 * Distributed under license. See LICENSE.TXT for details.
 *--
 */

#include "nametable.h"
#include "lexeme.h"
#include "scanner.h"
#include "listings.h"
#include "machinedef.h"
#include "support/logging.h"
#include "support/utils.h"

typedef enum {
    PUNCT_COMMASEP_NOGROUP,
    PUNCT_SEMISEP_NOGROUP,
    PUNCT_OPERSEP_NOGROUP,
    PUNCT_SEMISEP_SETTES,
    PUNCT_COMMASEP_PARENS
} punctclass_t;

struct parse_ctx_s;
typedef struct parse_ctx_s *parse_ctx_t;

typedef int (*lexfunc_t)(parse_ctx_t pctx, void *ctx, quotelevel_t ql,
                         lextype_t curlt);
parse_ctx_t parser_init(strctx_t strctx, namectx_t namectx, machinedef_t *mach,
                        scopectx_t *kwdscopep, logctx_t logctx, void *fioctx);
void parser_lexfunc_register(parse_ctx_t pctx, void *ctx,
                             lextype_t lt, lexfunc_t fn);
int parser_fopen_main(parse_ctx_t pctx, const char *fname, size_t fnlen,
                      unsigned int listopts, const char *listfname, size_t lfnlen);
int parser_fopen(parse_ctx_t pctx, const char *fname, size_t fnlen,
                 char **actnamep);
int parser_popen(parse_ctx_t pctx, scan_input_fn infn, void *fnctx);
int parser_lib_process(parse_ctx_t pctx, strdesc_t *libname);
void parser_finish(parse_ctx_t pctx);
lexctx_t parser_lexmemctx(parse_ctx_t pctx);
logctx_t parser_logctx(parse_ctx_t pctx);
lextype_t parser_next(parse_ctx_t pctx, quotelevel_t ql, lexeme_t **lex);
void parser_insert(parse_ctx_t pctx, lexeme_t *lex);
void parser_insert_seq(parse_ctx_t pctx, lexseq_t *seq);
void parser_skip_to_delim(parse_ctx_t pctx, lextype_t delimtype);
scopectx_t parser_scope_get(parse_ctx_t pctx);
scopectx_t parser_scope_push(parse_ctx_t pctx, scopectx_t newscope);
scopectx_t parser_scope_pop(parse_ctx_t pctx);
scopectx_t parser_scope_begin(parse_ctx_t pctx);
scopectx_t parser_scope_end(parse_ctx_t pctx);
void parser_incr_erroneof(parse_ctx_t pctx);
void parser_decr_erroneof(parse_ctx_t pctx);
void parser_skipmode_set(parse_ctx_t pctx, int val);
int parser_condstate_push(parse_ctx_t pctx, condstate_t newcs);
condstate_t parser_condstate_get(parse_ctx_t pctx);
int parse_lexeme_seq(parse_ctx_t pctx, lexseq_t *seq, quotelevel_t ql,
                     lextype_t terms[], int nterms,
                     lexseq_t *result, lextype_t *term);
lexeme_t *parser_lexeme_create(parse_ctx_t pctx, lextype_t lt, strdesc_t *txt);
int parser_expect(parse_ctx_t pctx, quotelevel_t ql,
                  lextype_t explt, lexeme_t **lex, int pboe);
int parser_expect_oneof(parse_ctx_t pctx, quotelevel_t ql,
                              lextype_t explts[], int numlts,
                              lexeme_t **lex, int pboe);
machinedef_t *parser_get_machinedef(parse_ctx_t pctx);
strctx_t parser_strctx(parse_ctx_t pctx);
textpos_t parser_curpos(parse_ctx_t pctx);
scopectx_t parser_kwdscope(parse_ctx_t pctx);
lstgctx_t parser_lstgctx(parse_ctx_t pctx);
void parser_punctclass_set(parse_ctx_t pctx, punctclass_t cl, lextype_t lt);
void parser_punctclass_get(parse_ctx_t pctx, punctclass_t *clp, lextype_t *ltp);
lexeme_t *parser_punct_grouper(parse_ctx_t pctx, int docloser);
lexeme_t *parser_punct_separator(parse_ctx_t pctx);
lexeme_t *parse_string_params(parse_ctx_t pctx, int openparenparsed);
void parser_variant_set(parse_ctx_t pctx, unsigned int val);
int parser_atend(parse_ctx_t pctx);
void parser_compilerinfo_set(parse_ctx_t pctx, compilerinfo_t *ci);
int parser_searchpath_add(parse_ctx_t pctx, strdesc_t *path);
unsigned int parser_searchpathcount_get(parse_ctx_t pctx);
strdesc_t *parser_searchpath_get(parse_ctx_t pctx, unsigned int i);
#endif /* parser_h__ */
