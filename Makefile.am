# Makefile for blissc
#
# Copyright (c) 2013, Matthew Madison.
# All rights reserved.
# Distributed under license.  See LICENSE.TXT for details.
#

AUTOMAKE_OPTIONS = subdir-objects foreign
AM_CPPFLAGS = -Iinclude -I$(srcdir)/include -I$(LLVM_INCLUDEDIR)
AM_CFLAGS = -Wall 

noinst_LIBRARIES = libdriver.a libfrontend.a libsupport.a libllvmgen.a
libdriver_a_SOURCES = lib/driver/driver.c
libfrontend_a_SOURCES = \
	lib/frontend/charfuncs.c \
	lib/frontend/declarations.c \
	lib/frontend/execfuncs.c \
	lib/frontend/expr_control.c \
	lib/frontend/expression.c \
	lib/frontend/lexeme.c \
	lib/frontend/lexer.c \
	lib/frontend/libgen.c \
	lib/frontend/listings.c \
	lib/frontend/macros.c \
	lib/frontend/nametable.c \
	lib/frontend/parser.c \
	lib/frontend/scanner.c \
	lib/frontend/structures.c \
	lib/frontend/switches.c \
	lib/frontend/symbols.c
libllvmgen_a_SOURCES = \
	lib/llvmgen/llvm_ctrlexpgen.c \
	lib/llvmgen/llvm_execfuncgen.c \
	lib/llvmgen/llvm_expgen.c \
	lib/llvmgen/llvm_gencode.c \
	lib/llvmgen/llvm_machines.c \
	lib/llvmgen/llvm_opexpgen.c \
	lib/llvmgen/llvm_symgen.c \
	lib/llvmgen/llvm_builtins_x86.c \
	lib/llvmgen/llvm_helper.cpp
libsupport_a_SOURCES = \
	lib/support/fileio.c \
	lib/support/logging.c \
	lib/support/statcodes.c \
	lib/support/strings.c \
	lib/support/utils.c

bin_PROGRAMS = blissc
blissc_SOURCES = driver/blissc.c
blissc_LDADD = $(noinst_LIBRARIES) $(LLVM_LIBS) -lstdc++ -lpthread -ldl
blissc_LDFLAGS = -L$(LLVM_LIBDIR)

SUBDIRS = tests
DIST_SUBDIRS =
EXTRA_DIST = \
	LICENSE.TXT README.md \
	include/blissc/charfuncs.h \
	include/blissc/declarations.h \
	include/blissc/driver.h \
	include/blissc/execfuncs.h \
	include/blissc/expression.h \
	include/blissc/gencode.h \
	include/blissc/lexeme.h \
	include/blissc/lexer.h \
	include/blissc/libgen.h \
	include/blissc/listings.h \
	include/blissc/machinedef.h \
	include/blissc/macros.h \
	include/blissc/nametable.h \
	include/blissc/parser.h \
	include/blissc/structures.h \
	include/blissc/switches.h \
	include/blissc/symbols.h \
	include/blissc/support/fileio.h \
	include/blissc/support/logging.h \
	include/blissc/support/statcodes.h \
	include/blissc/support/strings.h \
	include/blissc/support/utils.h \
	lib/frontend/scanner.h \
	lib/llvmgen/llvmgen.h \
	lib/llvmgen/llvm_helper.h \
	lib/llvmgen/llvm_machinectx.h \
	tests/runtests.py tests/testharness.req tests/testharness.c \
	tests/expression tests/lexical
