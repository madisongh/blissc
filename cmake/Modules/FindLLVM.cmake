# Copyright (c) 2018-2020, Matthew Madison
# Distributed under license.  See LICENSE.TXT for details.

#.rst:
#FindLLVM
#--------
#
# Finds LLVM
#
# This will define the following variables:
#
#   LLVM_FOUND            - True if LLVM is found
#   LLVM_VERSION_STRING   - The version of LLVM which was found
#   LLVM_HOST_TRIPLE      - The host triple LLVM was built with
#   LLVM_CFLAGS           - Compiler options for C modules calling LLVM
#   LLVM_CXXFLAGS         - Compiler options for C++ modules calling LLVM
#   LLVM_INCLUDE_DIRS     - Include directories for LLVM
#   LLVM_LDFLAGS          - Linker options for linking with LLVM
#   LLVM_LIBDIR           - Path to the LLVM libraries directory
#   LLVM_LIBRARIES        - Libraries to include when linking with LLVM
#   LLVM_SYSTEM_LIBS      - System libraries to link with when using LLVM
#
# Variables that control where to look:
#
#   LLVM_CONFIG - pathname of a working llvm-config program

if(NOT LLVM_CONFIG)
  find_program(LLVM_CONFIG NAMES llvm-config)
endif()

macro(_llvm_fail _msg)
  if(LLVM_FIND_REQUIRED)
    message(FATAL_ERROR "${_msg}")
  else()
    if(NOT LLVM_FIND_QUIETLY)
      message(STATUS "${_msg}")
    endif()
  endif()
endmacro()

if(NOT LLVM_CONFIG)
  if(NOT LLVM_FIND_QUIETLY)
    message(WARNING "Could not locate llvm-config executable. Set LLVM_CONFIG to point to a working version of that program.")
  endif()
else()
  macro(_llvm_set _var _flag _type _doc)
    if(LLVM_FIND_QUIETLY)
      set(_quiet ERROR_QUIET)
    else()
      set(_quiet)
    endif()
    set(_result)
    execute_process(
      COMMAND ${LLVM_CONFIG} --${_flag}
      RESULT_VARIABLE _result
      OUTPUT_VARIABLE _tmp
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ${_quiet})
    if(_result)
      _llvm_fail("llvm-config failed, result: ${_result}")
    endif()
    separate_arguments(_tmp)
    set(LLVM_${_var} ${_tmp} CACHE ${_type} ${_doc})
  endmacro()
  _llvm_set(VERSION_STRING version STRING "LLVM version")
  _llvm_set(CFLAGS cflags STRING "LLVM CFLAGS")
  _llvm_set(CXXFLAGS cxxflags STRING "LLVM CXXFLAGS")
  _llvm_set(INCLUDE_DIRS includedir PATH "LLVM include directories")
  _llvm_set(LDFLAGS ldflags STRING "LLVM LDFLAGS")
  _llvm_set(SYSTEM_LIBS system-libs STRING "LLVM system libraies")
  _llvm_set(LIBDIR libdir PATH "LLVM library directory")
  _llvm_set(LIBRARIES libs STRING "LLVM libraries")
  _llvm_set(HOST_TRIPLE host-target STRING "LLVM host triple")
endif()

if(${LLVM_VERSION_STRING} VERSION_LESS ${LLVM_FIND_VERSION})
  message(FATAL_ERROR "Found version ${LLVM_VERSION_STRING} of LLVM, need at least ${LLVM_FIND_VERSION}")
endif()

string(REGEX REPLACE "([0-9]+)\\..*" "\\1" LLVM_VERSION_MAJOR "${LLVM_VERSION_STRING}")
string(REGEX REPLACE "[0.9]+\\.([0-9]+).*" "\\1" LLVM_VERSION_MINOR "${LLVM_VERSION_STRING}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLVM
  REQUIRED_VARS LLVM_LIBRARIES
  VERSION_VAR LLVM_VERSION_STRING)
