#!/bin/bash
set -e
set -x

source $TESTROOTDIR/tests_lib

fury layer init
fury project add -n p1

fury module add -n m1
fury module add -n m2
fury module add -n m3

fury module select -m m1
fury dependency add -l m2

fury module select -m m2
fury dependency add -l m3

fury module select -m m3
fury dependency add -l m1

OUTPUT=$((! fury) | grep "Cycle in dependencies" )
EXPECTED="Cycle in dependencies : [p1/m3 -> p1/m1 -> p1/m2 -> p1/m3]"
assert_equal "$EXPECTED" "$OUTPUT"
