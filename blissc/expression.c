//
//  expression.c
//  blissc
//
//  Created by Matthew Madison on 11/1/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#include "expression.h"
#include "parser.h"
#include "lexeme.h"

int parse_Expression (parse_ctx_t pctx)
{
    lexeme_t *lex;

    parser_next(pctx, &lex);
    parser_insert(pctx, lex);
    
    return 1;
}