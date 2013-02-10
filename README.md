README for the BLISS-M Compiler
===============================

This is the source package for the BLISS-M compiler, a portable
cross-compiler for the BLISS programming language.
Visit the [project website](http://madisongh.github.com/blissc)
for more information about BLISS-M.

Current Status
--------------

Work in progress.   The front-end is mostly complete, with a fully
functioning parser and macro facility.  Back-end support is currently
limited to LLVM and x86-64 CPUs, with only a limited amount of BLISS
machine-specific support.

Prerequisites
-------------

Known to build on Mac OS X 10.8 (using Xcode 4.6) and Ubuntu 12.04.

The LLVM-based code generator was developed against LLVM 3.2, which
can be obtained [here](http://llvm.org/releases/download.html#3.2).
On a Mac, you can use the MacPorts 'llvm-3.2' package, but debugging
code generation issues is easier if you download the sources and
create a debug build yourself.

Building the compiler
---------------------

The supplied makefile, while relatively simple, does support building
the compiler outside of the source tree.

1. Clone [the repository](https://github.com/madisongh/blissc.git).
2. cd to your build directory.
3. With ${SRCTOP} being the location of the top of the source tree:
   `make -f ${SRCTOP}/Makefile srcdir=${SRCTOP}/` (note the ending
   slash on the specification of `srcdir`).

If `llvm-config` is not in your PATH, you'll need to specify its
location by adding `LLVM_CONFIG=path-to-llvm-config` on the `make`
command as well.

Running the compiler
--------------------

The build will produce a program called **blissc** in your build
directory.  Run `./blissc --help` for a description of the arguments
and options.

Contributions
-------------

There is a lot more yet to do on this project!  If you are interested
in contributing, contact me (madisongh on GitHub, or by e-mail at
madison _at_ bliss-m.org).

License
-------
All sources are released under the BSD 2-clause license.  See the
[LICENSE.TXT](https://github.com/madisongh/blissc/blob/master/LICENSE.TXT)
file for the license text.
