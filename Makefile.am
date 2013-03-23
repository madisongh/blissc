# Makefile for blissc
#
# Copyright (c) 2013, Matthew Madison.
# All rights reserved.
# Distributed under license.  See LICENSE.TXT for details.
#

AUTOMAKE_OPTIONS = subdir-objects foreign
AM_CPPFLAGS = -I$(srcdir)/include -I$(LLVM_INCLUDEDIR)
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

PYTHON = python
tests:
	mkdir tests

check: blissc | tests
	cd tests; $(PYTHON) $(abspath $(srcdir)/tests/runtests.py) --blissc=$(PWD)/blissc --cc=$(CC) $(abspath $(srcdir)/tests)
