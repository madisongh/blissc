//
//  strings.h
//  blissc
//
//  Created by Matthew Madison on 10/31/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_strings_h
#define blissc_strings_h

#include <string.h>
#include "utils.h"

int stringpool_init(void);
strdesc_t *string_from_chrs(strdesc_t *dest,
                            const char *cp, size_t len);
strdesc_t *ascic_string_from_chrs(strdesc_t *dest,
                                  const char *cp, size_t len);
strdesc_t *string_append(strdesc_t *trg, strdesc_t *add);
strdesc_t *string_alloc(strdesc_t *dest, size_t len);
strdesc_t *string_copy(strdesc_t *dest, strdesc_t *src);
void string_free(strdesc_t *dsc);

#endif
