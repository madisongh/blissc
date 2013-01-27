all: blissc

CPPFLAGS += -I$(srcdir)include
CFLAGS += -MMD -Wall 
CXXFLAGS += -MMD
LIBS = $(shell llvm-config --libs) -lstdc++ -lpthread -ldl

LIBDIRS := driver frontend llvmgen support

driver_SRCS := driver.c
frontend_SRCS := chf_generic.c declarations.c execfuncs.c \
	expr_control.c expression.c lexeme.c lexer.c listings.c \
	macros.c nametable.c parser.c scanner.c structures.c \
	switches.c symbols.c
llvmgen_SRCS := llvm_ctrlexpgen.c llvm_execfuncgen.c llvm_expgen.c \
	llvm_gencode.c llvm_machines.c llvm_opexpgen.c llvm_symgen.c
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

testdriver.o: $(srcdir)tests/testdriver.c
	$(COMPILE.c) $(OUTPUT_OPTION) $<

blissc: testdriver.o $(OBJS)
	$(CC) $(LDFLAGS) -o $@ testdriver.o $(OBJS) $(LIBS)

$(foreach dep,$(patsubst %.o,%.d,$(OBJS)),$(eval \
-include $(dep)))
-include testdriver.d