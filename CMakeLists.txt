cmake_minimum_required(VERSION 3.13)

project(blissc VERSION 0.3.90)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")

find_package(LLVM 14.0 REQUIRED)
find_package(Python3 COMPONENTS Interpreter)

include_directories(${blissc_BINARY_DIR}/include ${CMAKE_SOURCE_DIR}/include)

if(NOT MSVC)
  add_compile_options(-Wall -Wextra -Wno-unused-parameter -Wpedantic $<$<CONFIG:DEBUG>:-Werror> $<$<CONFIG:DEBUG>:-Wno-error=deprecated-declarations>)
endif()

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
add_executable(blissc driver/blissc.c)
target_link_libraries(blissc
  libdriver
  libfrontend
  libsupport
  libllvmgen
  ${LLVM_LIBRARIES}
  ${LLVM_SYSTEM_LIBS})
target_link_options(blissc PUBLIC ${LLVM_LDFLAGS} -Wl,-rpath=${LLVM_LIBDIR} -Wl,-rpath-link=${LLVM_LIBDIR})
configure_file(${CMAKE_SOURCE_DIR}/cmake/config.h.in ${blissc_BINARY_DIR}/include/blissc/config.h @ONLY)

if (${Python3_FOUND})
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/tests)
add_custom_target(testharness
        COMMAND $<TARGET_FILE:blissc> --library -o ${CMAKE_BINARY_DIR}/tests/testharness.lib ${CMAKE_SOURCE_DIR}/tests/testharness.req
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests
        DEPENDS blissc)

add_custom_target(check ALL
        COMMAND python3 "${CMAKE_SOURCE_DIR}/tests/runtests.py" --blissc=$<TARGET_FILE:blissc> --cc="${CMAKE_C_COMPILER}" "${CMAKE_SOURCE_DIR}/tests"
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests
        DEPENDS testharness blissc)
else()
  message(No Python3 - omitting compiler tests)
endif()
