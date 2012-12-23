#ifndef fileio_h__
#define fileio_h__
/*
 *++
 *	File:			fileio.h
 *
 *	Abstract:		File I/O definitions.
 *
 *	Author:			M. Madison
 *					Copyright © 2012, Matthew Madison
 *					All rights reserved.
 *--
 */
#include <stdio.h>
#include "logging.H"

struct filectx_s;
typedef struct filectx_s *filectx_t;
struct fioctx_s;
typedef struct fioctx_s *fioctx_t;

fioctx_t fileio_init(logctx_t logctx);
void fileio_finish(fioctx_t fio);

filectx_t file_open_input(fioctx_t fio, const char *fname,
                          size_t fnlen, const char *suffix);
void file_close(filectx_t ctx);
char *file_getname(filectx_t ctx);
int file_readline(filectx_t ctx, char *buf, size_t bufsiz, size_t *len);

#endif /* fileio_h__ */
