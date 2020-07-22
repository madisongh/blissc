# README for the BLISS-M Compiler

This is the source package for the BLISS-M compiler, a portable
cross-compiler for the BLISS programming language.
Visit the [project website](http://madisongh.github.io/blissc)
for more information about BLISS-M.

## Current Status

Work in progress.   The front-end is mostly complete, with a fully
functioning parser and macro facility.  Back-end support is currently
limited to LLVM and x86 CPUs (32- or 64-bit), with a limited amount
of machine-specific support.

## Prerequisites

* Recent-vintage C compiler
* Recent version LLVM
* CMake 3.13 or later, or a recent version of autotools

Recent development and testing has been on Ubuntu 20.04 with
gcc 9 and LLVM 9.0.1 (prebuilt llvm-9-dev package) and LLVM
10.0.1 (built from sources).

## Building the compiler

First, clone [the repository](https://github.com/madisongh/blissc.git).

### Using Autotools
1. cd to the top-level source directory and run `autoreconf -i`.
2. If you want to build outside the source tree, cd to your
   build directory.
3. Run the `configure` script that was generated in step 2.  If
   `llvm-config` is not in your PATH, use the `--with-llvm-config`
   option on `configure` to specify its location.
4. Run `make` to build the compiler.
5. Run `make check` to test the built compiler.

### Using CMake
1. Create a directory for the build.
2. Run `cmake <path-to-sources>` to generate the build files.
   CMake will attempt to locate a usable copy of LLVM automatically,
   but you can specify -DLLVM_CONFIG=<path-to-llvm-config> to force
   it to use a particular installation of LLVM.
3. Run `make` to build the compiler.

Running the compiler
--------------------

The build will produce a program called **blissc** in your build
directory.  Run `./blissc --help` for a description of the arguments
and options.


Contributions
-------------

There is a lot more yet to do on this project!  If you are interested
in contributing, contact me (madisongh on GitHub, or by e-mail at
matt _at_ madison _dot_ systems).

License
-------
All sources are released under the BSD 2-clause license.  See the
[LICENSE.TXT](https://github.com/madisongh/blissc/blob/master/LICENSE.TXT)
file for the license text.
