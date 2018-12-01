cmake_minimum_required(VERSION 3.8)

project(blissc VERSION 0.2)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")

find_package(LLVM 4.0 REQUIRED)

include_directories(${blissc_BINARY_DIR}/include ${CMAKE_SOURCE_DIR}/include)

add_library(libdriver STATIC lib/driver/driver.c)
add_library(libfrontend STATIC
  lib/frontend/charfuncs.c
  lib/frontend/declarations.c
  lib/frontend/execfuncs.c
  lib/frontend/expr_control.c
  lib/frontend/expression.c
  lib/frontend/lexeme.c
  lib/frontend/lexer.c
  lib/frontend/libgen.c
  lib/frontend/listings.c
  lib/frontend/macros.c
  lib/frontend/nametable.c
  lib/frontend/parser.c
  lib/frontend/scanner.c
  lib/frontend/structures.c
  lib/frontend/switches.c
  lib/frontend/symbols.c)
add_library(libllvmgen STATIC
  lib/llvmgen/llvm_ctrlexpgen.c
  lib/llvmgen/llvm_execfuncgen.c
  lib/llvmgen/llvm_expgen.c
  lib/llvmgen/llvm_gencode.c
  lib/llvmgen/llvm_machines.c
  lib/llvmgen/llvm_opexpgen.c
  lib/llvmgen/llvm_symgen.c
  lib/llvmgen/llvm_builtins_x86.c
  lib/llvmgen/llvm_helper.cpp)
target_compile_options(libllvmgen
  PRIVATE $<$<COMPILE_LANGUAGE:CXX>:-Wno-unknown-pragmas ${LLVM_CXXFLAGS}>
  PRIVATE $<$<COMPILE_LANGUAGE:C>:${LLVM_CFLAGS}>)
add_library(libsupport STATIC
  lib/support/fileio.c
  lib/support/logging.c
  lib/support/statcodes.c
  lib/support/strings.c
  lib/support/utils.c)
add_library(libir STATIC lib/ir/blissir.c)
add_executable(blissc driver/blissc)
target_link_libraries(blissc
  libdriver
  libfrontend
  libsupport
  libllvmgen
  libir
  ${LLVM_LIBRARIES}
  -lstdc++
  ${LLVM_SYSTEM_LIBRARIES})
set_property(TARGET blissc PROPERTY LINK_FLAGS ${LLVM_LDFLAGS})
configure_file(${CMAKE_SOURCE_DIR}/cmake/config.h.in ${blissc_BINARY_DIR}/include/blissc/config.h @ONLY)
