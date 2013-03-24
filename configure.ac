dnl ++
dnl  configure.ac - autoconf script for blissc
dnl
dnl  Copyright (c) 2013, Matthew Madison.
dnl  All rights reserved.
dnl
dnl  Distributed under license.  See LICENSE.TXT for details.
dnl --

AC_INIT([blissc], [0.1], [http://github.com/madisongh/blissc/issues])
AM_INIT_AUTOMAKE([subdir-objects foreign])
AM_SILENT_RULES([yes])
AC_COPYRIGHT([Copyright (c) 2013, Matthew Madison])

AC_PREREQ(2.69)

dnl Locate the LLVM installation through use of the llvm-config script.
dnl If the path is not provided by the user, assume it is in the path.

AC_ARG_WITH([llvm],
        AS_HELP_STRING([--with-llvm-config@<:@=PATH@:>@], [specify llvm-config script location]),
        AS_IF(["$withval" = "yes"], [ac_llvm_config=`which llvm-config`],
                          [ac_llvm_config="$withval"]),
        [ac_llvm_config=`which llvm-config`])
AS_IF([test -e "$ac_llvm_config"],[
          export ac_llvm_includedir=`$ac_llvm_config --includedir`
          export ac_llvm_libdir=`$ac_llvm_config --libdir`
          export ac_llvm_libs=`$ac_llvm_config --libs`],
          AC_MSG_ERROR([[could not find working llvm-config]]))
AC_SUBST(LLVM_INCLUDEDIR,[$ac_llvm_includedir])
AC_SUBST(LLVM_LIBDIR,[$ac_llvm_libdir])
AC_SUBST(LLVM_LIBS,[$ac_llvm_libs])

AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB

AC_CONFIG_FILES([Makefile tests/Makefile])
AC_CONFIG_HEADERS([include/blissc/config.h:config.h.in])
AC_OUTPUT
