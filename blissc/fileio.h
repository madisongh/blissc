//
//  fileio.h
//  blissc
//
//  Created by Matthew Madison on 10/22/12.
//  Copyright (c) 2012 Matthew Madison. All rights reserved.
//

#ifndef blissc_fileio_h
#define blissc_fileio_h

#include <stdio.h>

struct filectx_s;
typedef struct filectx_s *filectx_t;

void fileio_init(void);
void fileio_finish(void);

filectx_t file_open_input(const char *fname, size_t fnlen);
void file_close(filectx_t ctx);
int file_readline(filectx_t ctx, char *buf, size_t bufsiz, size_t *len);

#endif
