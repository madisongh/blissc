# Makefile for blissc
#
# Copyright (c) 2013, Matthew Madison.
# All rights reserved.
# Distributed under license.  See LICENSE.TXT for details.
#
#
# This file is temporary, and will be replaced when
# the package is autotooled.
#

all: blissc

LLVM_CONFIG = llvm-config
CPPFLAGS += -I$(srcdir)include -I$(shell $(LLVM_CONFIG) --includedir)
CFLAGS += -MMD -Wall 
CXXFLAGS += -MMD
LIBS = -L $(shell $(LLVM_CONFIG) --libdir) $(shell $(LLVM_CONFIG) --libs) \
	-lstdc++ -lpthread -ldl

LIBDIRS := driver frontend llvmgen support

driver_SRCS := driver.c
frontend_SRCS := charfuncs.c declarations.c execfuncs.c \
	expr_control.c expression.c lexeme.c lexer.c listings.c \
	macros.c nametable.c parser.c scanner.c structures.c \
	switches.c symbols.c
llvmgen_SRCS := llvm_ctrlexpgen.c llvm_execfuncgen.c llvm_expgen.c \
	llvm_gencode.c llvm_machines.c llvm_opexpgen.c llvm_symgen.c \
	llvm_builtins_x86.c
llvmgen_CXXSRCS := llvm_helper.cpp
support_SRCS := fileio.c logging.c statcodes.c strings.c utils.c

lib:
	mkdir -p lib

OBJS =
define librule
lib/$(1): | lib
	mkdir -p lib/$(1)
lib/$(1)/%.o: $(srcdir)lib/$(1)/%.c | lib/$(1)
	$$(COMPILE.c) $$(OUTPUT_OPTION) $$<
lib/$(1)/%.o: $(srcdir)lib/$(1)/%.cpp | lib/$(1)
	$$(COMPILE.cpp) $$(OUTPUT_OPTION) $$<
$(1)_OBJS := $(addprefix lib/$(1)/,$(patsubst %.c,%.o,$($(1)_SRCS)) \
		  $(patsubst %.cpp,%.o,$($(1)_CXXSRCS)))
OBJS += $$($(1)_OBJS)
endef

$(foreach libdir,$(LIBDIRS),$(eval $(call librule,$(libdir))))

blissc.o: $(srcdir)driver/blissc.c
	$(COMPILE.c) $(OUTPUT_OPTION) $<

blissc: blissc.o $(OBJS)
	$(CC) $(LDFLAGS) -o $@ blissc.o $(OBJS) $(LIBS)

$(foreach dep,$(patsubst %.o,%.d,$(OBJS)),$(eval \
-include $(dep)))
-include blissc.d