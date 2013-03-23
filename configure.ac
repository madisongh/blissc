AC_INIT([blissc], [0.1])
AM_INIT_AUTOMAKE([subdir-objects foreign])
AM_SILENT_RULES([yes])
AC_ARG_WITH([llvm],
        AS_HELP_STRING([--with-llvm@<:@=DIR@:>@], [specify location for LLVM]),
        AS_IF(["$withval" = "yes"], [ac_llvm_config=`which llvm-config`],
                          [ac_llvm_config="$withval"]),
        [ac_llvm_config=`which llvm-config`])
AS_IF([test -e "$ac_llvm_config"],[
          export LLVM_INCLUDEDIR=`$ac_llvm_config --includedir`
          export LLVM_LIBDIR=`$ac_llvm_config --libdir`
          export LLVM_LIBS=`$ac_llvm_config --libs`],
          AC_MSG_ERROR([[could not find working llvm-config]]))
AC_SUBST([LLVM_INCLUDEDIR])
AC_SUBST([LLVM_LIBDIR])
AC_SUBST([LLVM_LIBS])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_HEADERS([include/blissc/config.h:config.h.in])
AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB
AC_OUTPUT
