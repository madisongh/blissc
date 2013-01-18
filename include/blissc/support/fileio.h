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
#include <string.h>
#include "logging.h"

struct filectx_s;
typedef struct filectx_s *filectx_t;
struct fioctx_s;
typedef struct fioctx_s *fioctx_t;

fioctx_t fileio_init(logctx_t logctx);
void fileio_finish(fioctx_t fio);

struct fio_pathparts_s {
    char        *path_fullname;
    size_t       path_fullnamelen;
    char        *path_dirname;
    size_t       path_dirnamelen;
    char        *path_filename;
    size_t       path_filenamelen;
    char        *path_suffix;
    size_t       path_suffixlen;
};
typedef struct fio_pathparts_s fio_pathparts_t;

char *file_canonicalname(fioctx_t fio, const char *orig, int origlen, unsigned int *lenp);
int file_splitname(fioctx_t fio, const char *orig, int origlen, int canoncialize,
                   fio_pathparts_t *parts);
int file_combinename(fioctx_t fio, fio_pathparts_t *parts);
void file_freeparts(fioctx_t fio, fio_pathparts_t *parts);

filectx_t file_open_input(fioctx_t fio, const char *fname, size_t fnlen);
filectx_t file_open_output(fioctx_t fio, const char *fname, size_t fnlen);
void file_close(filectx_t ctx);
char *file_getname(filectx_t ctx);
int file_readline(filectx_t ctx, char *buf, size_t bufsiz, size_t *len);
int file_writeline(filectx_t ctx, const char *buf, size_t buflen);

#endif /* fileio_h__ */
