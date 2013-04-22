#!/usr/bin/env python
# runtests
#
#   Runs BLISS compiler tests.
#
#   Tests are BLISS source files with comments embedded that
#   indicate the expected results.
#
#   N.B.: requires Python 2.7 or later.
#
# Copyright (c) 2013, Matthew Madison
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
        print "Processing: {}".format(testfile)
    compout = []
    comperrs = []
    runout = []
    failures = 0
    cases = 0
    f = file(testfile, 'rU')
    for lraw in f.readlines():
        l = lraw.strip()
        if l.startswith(prefix):
            s = l[len(prefix):].strip()
            (cmd, junk, result) = s.partition(' ')
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
                    runout.append('TEST {:03d}: {}'.format(caseno,result.strip()))
    f.close()

    barename = os.path.splitext(os.path.basename(testfile))[0]
    docompile = subprocess.Popen([blissc, '-o', barename + '.o',
                                  '-I', '.', testfile],
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 universal_newlines=True)
    (oraw, eraw) = docompile.communicate()
    o = oraw.split('\n')
    e = []
    for l in eraw.split('\n'):
        if l.startswith('%'):
            e.append(l)

    for s in compout:
        if s in o:
            o.remove(s)
        else:
            failures = failures + 1
            if not quiet:
                print "  FAIL: compiler output [{}] not found".format(s)
    if len(e) != 0 and len(comperrs) == 0:
        failures = failures + 1
        if not quiet:
            print "  FAIL: unexpected compilation errors:"
            for l in e:
                print "         {}".format(l)
    else:
        for s in comperrs:
            if len(e) == 0:
                failures = failures + 1
                if not quiet:
                    print "  FAIL: expected compilation error(s) not found"
                break
            cur = e[0]
            e = e[1:]
            if not cur.startswith(s):
                failures = failures + 1
                if not quiet:
                    print "  FAIL: unmatched error: {}".format(s)
                    print "        actual: {}".format(cur)
    if not quiet and failures == 0:
        print "  compilation phase passed"

    subprocess.call([cc, '-o', barename, testharness, barename + '.o'])
    dorun = subprocess.Popen(['./' + barename], stdout=subprocess.PIPE,
                             universal_newlines=True)
    oraw = dorun.communicate()[0]
    o = oraw.split('\n')
    passes = 0
    for s in runout:
        if s in o:
            o.remove(s)
            passes = passes + 1
        else:
            failures = failures + 1
            pfx = s.partition(':')
            actual = ""
            for l in o:
                if l.startswith(pfx):
                    actual = l
                    break
            if actual == "":
                actual = "<not found>"
            else:
                o.remove(actual)
            if not quiet:
                print "   FAIL: wanted: {}".format(s)
                print "         actual: {}".format(actual)

    if not quiet:
        print "  {:d} of {:d} test cases passed".format(passes,cases)
    return (cases, passes, failures)


# begin main

(mydir, myname) = os.path.split(sys.argv[0])
parser = argparse.ArgumentParser(description='Run blissc unit tests', prog=myname)
parser.add_argument('testorsuite', help='test file name or test suite directory', nargs='*', default='.')
parser.add_argument('--blissc', help='location of BLISS compiler', default='blissc')
parser.add_argument('--harness', help='test harness C file', default=os.path.join(mydir, 'testharness.c'))
parser.add_argument('--cc', help="C compiler", default='cc')
parser.add_argument('--quiet', '-q', action='store_true')

args = parser.parse_args()

pat = re.compile('^.+\.bli$')
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
            print "Skipping non-existent test: {}".format(t_or_s)
        skipped = skipped + 1

totcases = 0
totpasses = 0
for t in tests:
    tried = tried + 1
    (c, p, f) = process_test(t, args.blissc, args.cc, args.harness, args.quiet)
    totcases = totcases + c
    totpasses = totpasses + p
    if f == 0:
        passed = passed + 1
    else:
        failed = failed + 1

if not args.quiet:
    print "-- Tried: {:d}; Skipped: {:d}; Passed: {:d}; Failed: {:d} --".format(tried, skipped, passed, failed)
    print "-- Total test cases: {:d} ({:d} passing) --".format(totcases, totpasses)
if tried == 0 or failed != 0:
    sys.exit(1)
sys.exit(0)
