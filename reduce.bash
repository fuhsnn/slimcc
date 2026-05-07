#!/bin/bash

# Preparing:
# clang -I ./ alloc.c hashmap.c main.c parse.c preprocess.c strings.c tokenize.c type.c unicode.c -shared -fPIC -Wno-switch -O2 -o amalg.so
# cp codegen.c work.c
#
# Fill this:
BASEDIR=

# GCC 15 or 16
GCCBIN=gcc-16

# Run:
# chmod +x reduce.bash
# cvise reduce.bash work.c

set -u

TEMPDIR=`mktemp -d /tmp/redu-XXXXXX`
trap 'rm -rf $TEMPDIR' INT TERM HUP EXIT

export ASAN_OPTIONS=detect_leaks=0

compile_args=(
 -Wall
 -Wno-overflow
 -Wno-unused
 -Wno-bool-compare
 -Wno-bool-operation
 -Wno-tautological-compare
 -Wno-address
 -Wno-pointer-sign
 -Wno-format
 -Wno-switch
 -Wno-parentheses
 -Wno-dangling-else
 -Wno-misleading-indentation
 -Wno-infinite-recursion
 -Wno-enum-compare
 -std=c23
 -pipe
 -fsanitize=address,undefined -fno-sanitize-recover=undefined
 -Wl,-rpath=$BASEDIR $BASEDIR/amalg.so
 -include $BASEDIR/slimcc.h
 work.c
)

grep_pat=(
-e 'Wmissing'
-e 'Warray-bounds'
-e 'Wzero-length'
-e 'past the end of the'
-e 'assumed to have one element'
-e 'no semicolon at end of struct or union'
-e 'Wdiscarded-qualifiers'
-e 'Wsequence-point'
-e 'Wincompatible'
-e 'Wreturn'
-e 'Wuninit'
-e 'Wmaybe-uninitialized'
)

chk() {

grep "${grep_pat[@]}" $1 -q
if (($? == 0)) then
   exit 1
fi

}

opt_args=(
-fno-auto-inc-dec
-fno-branch-count-reg
-fno-combine-stack-adjustments
-fno-compare-elim
-fno-cprop-registers
-fno-dce
-fno-defer-pop
-fno-dse
-fno-forward-propagate
-fno-guess-branch-probability
-fno-if-conversion
-fno-if-conversion2
-fno-inline-functions-called-once
-fno-ipa-modref
-fno-ipa-profile
-fno-ipa-pure-const
-fno-ipa-reference
# -fno-ipa-reference-addressable
-fno-ivopts
-fno-merge-constants
-fno-move-loop-invariants
-fno-move-loop-stores
-fno-omit-frame-pointer
-fno-reorder-blocks
-fno-shrink-wrap
-fno-shrink-wrap-separate
-fno-split-wide-types
-fno-ssa-backprop
-fno-ssa-phiopt
-fno-tree-bit-ccp
# -fno-tree-ccp
-fno-tree-ch
-fno-tree-coalesce-vars
-fno-tree-copy-prop
-fno-tree-dce
-fno-tree-dominator-opts
-fno-tree-dse
-fno-tree-forwprop
-fno-tree-fre
-fno-tree-phiprop
-fno-tree-pta
-fno-tree-scev-cprop
-fno-tree-sink
-fno-tree-slsr
-fno-tree-sra
-fno-tree-ter
# -fno-unit-at-a-time
)


$GCCBIN -flto=1 -Og ${opt_args[@]} -o $TEMPDIR/badout ${compile_args[@]} > $TEMPDIR/gccwarns 2>&1
if [ $? -ne 0 ]; then
  exit 1
fi

chk $TEMPDIR/gccwarns

timeout 0.1 $TEMPDIR/badout -S $BASEDIR/bug.c -o/dev/null -nostdinc >/dev/null
if [ $? -ne 11 ]; then
  exit 1
fi

$GCCBIN -O2 -fno-inline -o $TEMPDIR/gccout ${compile_args[@]} > $TEMPDIR/gccwarns 2>&1
if [ $? -ne 0 ]; then
  exit 1
fi

chk $TEMPDIR/gccwarns

timeout 0.1 $TEMPDIR/gccout -S $BASEDIR/bug.c -o/dev/null -nostdinc >/dev/null
if [ $? -ne 10 ]; then
  exit 1
fi
