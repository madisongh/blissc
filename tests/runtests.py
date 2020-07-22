#!/usr/bin/env python3
# runtests
#
#   Runs BLISS compiler tests.
#
#   Tests are BLISS source files with comments embedded that
#   indicate the expected results.
#
#   N.B.: requires Python 3.
#
# Copyright (c) 2013-2020, Matthew Madison
# All rights reserved.
# Distributed under license; see LICENSE.TXT for details.

import sys
import os
import argparse
import re
import subprocess

tests = []
skipped = 0
tried = 0
passed = 0
failed = 0


def process_test(testfile, blissc, cc, testharness, quiet=False, prefix='!!'):
    if not quiet:
        print("Processing: {}".format(testfile))
    compout = []
    comperrs = []
    runout = []
    failures = 0
    cases = 0
    with open(testfile, 'r') as infile:
        caselist = [line.rstrip() for line in infile.readlines() if line.startswith(prefix)]
    for case in caselist:
        s = case[len(prefix):].strip()
        cmd, junk, result = s.partition(' ')
        if cmd == 'cout':
            compout.append(result.strip())
        elif cmd == 'cerr':
            comperrs.append(result.strip())
        else:
            try:
                caseno = int(cmd)
            except ValueError:
                caseno = 0
            if caseno != 0:
                cases = cases + 1
                runout.append('TEST {:03d}: {}'.format(caseno, result.strip()))

    barename = os.path.splitext(os.path.basename(testfile))[0]
    try:
        docompile = subprocess.run([blissc, '-o', barename + '.o', '-I', '.', testfile],
                                   capture_output=True, text=True, check=True)
        o = docompile.stdout.split('\n')
        if docompile.stderr:
            e = [line for line in docompile.stderr.split('\n') if line.startswith('%')]
        else:
            e = []
    except subprocess.CalledProcessError as err:
        if err.returncode > 0 and len(comperrs) > 0:
            o = err.stdout.split('\n')
            e = [line for line in err.stderr.split('\n') if line.startswith('%')]
            pass
        else:
            print(("  FAIL: compilation failed ({}) for {}" +
                   "\n        {}").format(err.returncode, testfile,
                                          '\n        '.join([line for line in err.stderr.split('\n')])))
            return cases, 0, cases

    for s in compout:
        if s in o:
            o.remove(s)
        else:
            failures = failures + 1
            if not quiet:
                print("  FAIL: compiler output [{}] not found".format(s))
    if len(e) != 0 and len(comperrs) == 0:
        failures = failures + 1
        if not quiet:
            print("  FAIL: unexpected compilation errors:")
            for errline in e:
                print("         {}".format(errline))
    else:
        for s in comperrs:
            if len(e) == 0:
                failures = failures + 1
                if not quiet:
                    print("  FAIL: expected compilation error(s) not found")
                break
            cur = e[0]
            e = e[1:]
            if not cur.startswith(s):
                failures = failures + 1
                if not quiet:
                    print("  FAIL: unmatched error: {}".format(s))
                    print("        actual: {}".format(cur))
    if not quiet and failures == 0:
        print("  compilation phase passed")

    try:
        subprocess.run([cc, '-o', barename, testharness, barename + '.o'], check=True)
    except subprocess.CalledProcessError as e:
        print(("  FAIL: linking {} into test harness failed" +
              "\n        {}").format(barename + '.o', '\n        '.join([line for line in e.stderr.split('\n')])))
        return cases, 0, cases
    try:
        dorun = subprocess.run(['./' + barename], capture_output=True, text=True, check=True)
    except subprocess.CalledProcessError as e:
        print(("  FAIL: test harness failed to run for {}" +
              "\n        {}").format(testfile, '\n        '.join([line for line in e.stderr.split('\n')])))
        return cases, 0, cases

    o = dorun.stdout.split('\n')
    passes = 0
    for s in runout:
        if s in o:
            o.remove(s)
            passes = passes + 1
        else:
            failures = failures + 1
            pfx = s.partition(':')
            actual = ""
            for outline in o:
                if outline.startswith(pfx):
                    actual = outline
                    break
            if actual == "":
                actual = "<not found>"
            else:
                o.remove(actual)
            if not quiet:
                print("   FAIL: wanted: {}".format(s))
                print("         actual: {}".format(actual))

    if not quiet:
        print("  {:d} of {:d} test cases passed".format(passes, cases))
    return cases, passes, failures


# begin main

mydir, myname = os.path.split(sys.argv[0])
parser = argparse.ArgumentParser(description='Run blissc unit tests', prog=myname)
parser.add_argument('testorsuite', help='test file name or test suite directory', nargs='*', default='.')
parser.add_argument('--blissc', help='location of BLISS compiler', default='blissc')
parser.add_argument('--harness', help='test harness C file', default=os.path.join(mydir, 'testharness.c'))
parser.add_argument('--cc', help="C compiler", default=os.getenv("CC") or "cc")
parser.add_argument('--quiet', '-q', action='store_true')

args = parser.parse_args()

pat = re.compile(r'^.+\.bli$')
for t_or_s in args.testorsuite:
    if os.path.exists(t_or_s):
        if os.path.isdir(t_or_s):
            for d, sdl, fl in os.walk(t_or_s):
                for f in fl:
                    if pat.match(f):
                        tests.append(os.path.join(d, f))
        else:
            tests.append(t_or_s)
    else:
        if not args.quiet:
            print("Skipping non-existent test: {}".format(t_or_s))
        skipped = skipped + 1

totcases = 0
totpasses = 0
for t in tests:
    tried = tried + 1
    c, p, f = process_test(t, args.blissc, args.cc, args.harness, args.quiet)
    totcases = totcases + c
    totpasses = totpasses + p
    if f == 0:
        passed = passed + 1
    else:
        failed = failed + 1

if not args.quiet:
    print("-- Tried: {:d}; Skipped: {:d}; Passed: {:d}; Failed: {:d} --".format(tried, skipped, passed, failed))
    print("-- Total test cases: {:d} ({:d} passing) --".format(totcases, totpasses))
if tried == 0 or failed != 0:
    sys.exit(1)
sys.exit(0)
