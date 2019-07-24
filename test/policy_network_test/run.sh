#!/bin/bash
set -e
set -x

source $TESTROOTDIR/tests_lib

fury layer init
fury project add -n policy-network-test
fury repo add -u https://github.com/propensive/base.git -n base
fury import add -i base:2.12.6

# Try sending a request to an external server
fury module add -n hello-internet
fury source add -d src
fury module update -c scala/compiler
fury module update --type application
fury module update --main test.HelloInternet
fury
