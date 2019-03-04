#!/bin/bash
set -e
set -x

source $TESTROOTDIR/tests_lib

fury layer init
mkdir -p foo/bar/baz
cd foo/bar/baz
fury project add -n webpage
fury module add -n hello-world
fury source add -d src
fury source list
fury repo add -u https://github.com/propensive/base.git -n base
fury import add -i base:2.12.6
fury module update --compiler scala/compiler
fury module update --type application
fury module update --main HelloWorld
fury
cd ../../..

# Test JAR file validity
fury build save --dir ./

OUTPUT=$(java -cp "$SCALA:webpage-hello-world.jar" "HelloWorld")
EXPECTED="Hello, world!"
assert_equal "$EXPECTED" "$OUTPUT"

# Test native-image file validity
export PATH="${GRAAL_HOME}/bin:${PATH}"
fury restart
mkdir -p native
fury build native --dir native

OUTPUT=$(./native/helloworld)
EXPECTED="Hello, world!"
assert_equal "$EXPECTED" "$OUTPUT"
