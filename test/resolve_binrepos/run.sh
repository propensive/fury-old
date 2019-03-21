#!/bin/bash
set -e
set -x

source $TESTROOTDIR/tests_lib

fury layer init
fury project add -n webpage
fury module add -n hello-jackson
fury source add -d src
fury source list
fury repo add -u https://github.com/propensive/base.git -n base
fury import add -i base:2.12.6
fury binary add -b com.fasterxml.jackson.module:jackson-module-scala_2.12:2.9.9-SNAPSHOT -r sonatype:snapshots
fury module update --compiler scala/compiler
fury module update --type application
fury module update --main HelloJackson
fury

fury build classpath

mkdir target

# Test JAR file validity
fury build save --dir target

EXPECTED="class com.fasterxml.jackson.databind.ObjectMapper"

OUTPUT=$(java -cp "$SCALA:target/*" "HelloJackson")
assert_equal "$EXPECTED" "$OUTPUT"

# Test native-image file validity
export PATH="${GRAAL_HOME}/bin:${PATH}"
fury restart
mkdir -p native
fury build native --dir native

OUTPUT=$(./native/hellojackson)
assert_equal "$EXPECTED" "$OUTPUT"
