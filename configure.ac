dnl ++
dnl  configure.ac - autoconf script for blissc
dnl
dnl  Copyright (c) 2013-2024, Matthew Madison.
dnl  All rights reserved.
dnl
dnl  Distributed under license.  See LICENSE.TXT for details.
dnl --

AC_INIT([blissc], [0.5.0], [http://github.com/madisongh/blissc/issues])
AC_DEFINE([BLISSC_VERSION_MAJOR], [0], [Major version of blissc])
AC_DEFINE([BLISSC_VERSION_MINOR], [5], [Minor version of blissc])
AC_DEFINE([BLISSC_VERSION_MAINT], [0], [Maintenance level of blissc])
AM_INIT_AUTOMAKE([subdir-objects foreign])
AM_SILENT_RULES([yes])
AC_COPYRIGHT([Copyright (c) 2013-2024, Matthew Madison])

AC_PREREQ(2.69)

AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED([BLISSC_HOST_TRIPLE], ["$host"], [Compiler host triple])

dnl Locate the LLVM installation through use of the llvm-config script.
dnl If the path is not provided by the user, assume it is in the path.

AC_ARG_WITH([llvm-config],
        AS_HELP_STRING([--with-llvm-config@<:@=PATH@:>@], [specify llvm-config script location]),
        AS_IF([test "$withval" = "yes"], [ac_llvm_config=`which llvm-config`],
                          [ac_llvm_config="$withval"]),
        [ac_llvm_config=`which llvm-config`])
AS_IF([test -e "$ac_llvm_config"],[
          export ac_llvm_includedir=`$ac_llvm_config --includedir`
          export ac_llvm_libdir=`$ac_llvm_config --libdir`
          export ac_llvm_cflags=`$ac_llvm_config --cflags`
          export ac_llvm_cxxflags=`$ac_llvm_config --cxxflags`
          export ac_llvm_ldflags=`$ac_llvm_config --ldflags`
          export ac_llvm_libs=`$ac_llvm_config --libs`
          export ac_llvm_syslibs=`$ac_llvm_config --system-libs 2>/dev/null | grep -v '^ *$'`],
          AC_MSG_ERROR([[could not find working llvm-config]]))
AC_SUBST(LLVM_INCLUDEDIR,[$ac_llvm_includedir])
AC_SUBST(LLVM_LIBDIR,[$ac_llvm_libdir])
AC_SUBST(LLVM_LIBS,[$ac_llvm_libs])
AC_SUBST(LLVM_SYSLIBS,[$ac_llvm_syslibs])
AC_SUBST(LLVM_CFLAGS,[$ac_llvm_cflags])
AC_SUBST(LLVM_CXXFLAGS,[$ac_llvm_cxxflags])
AC_SUBST(LLVM_LDFLAGS,[$ac_llvm_ldflags])

AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB
AM_PATH_PYTHON([3.0],,[:])
AM_CONDITIONAL([HAVE_PYTHON], [test "$PYTHON" != :])

AC_CONFIG_FILES([Makefile tests/Makefile])
AC_CONFIG_HEADERS([include/blissc/config.h:config.h.in])
AC_OUTPUT
