# README for the BLISS-M Compiler

This is the source package for the BLISS-M compiler, a portable
cross-compiler for the BLISS programming language.

## What is BLISS?
The BLISS language was developed at Carnegie-Mellon University in the 1970's for systems programming.
It was adopted and used extensively by Digital Equipment Corporation for development of systems
software across their 16-bit, 32-bit, 36-bit, and 64-bit systems.  It is a typeless, block-structured,
language with an extensive lexical processing (macro) facility.  See the
[Wikipedia article on BLISS](http://en.wikipedia.org/wiki/BLISS) for more information about the language.


## Current Status

Work in progress.   The front-end is mostly complete, with a fully
functioning parser and macro facility.  Back-end support is currently
limited to LLVM and x86 CPUs (32- or 64-bit), with a limited amount
of machine-specific support.

## Prerequisites

* Recent-vintage C compiler
* Recent version LLVM (one that has support for opaque pointers)
* CMake 3.13 or later, or a recent version of autotools

Recent development and testing has been on Ubuntu 22.04 with
gcc 11 and LLVM 15.0.

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


License
-------
All sources are released under the BSD 2-clause license.  See the
[LICENSE.TXT](https://github.com/madisongh/blissc/blob/master/LICENSE.TXT)
file for the license text.
