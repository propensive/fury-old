#!/usr/bin/env bash

FURY_VERSION=test

echo "     _____"
echo "    / ___/__ __ ____ __ __"
echo "   / __/ / // // ._// // /"
echo "  /_/    \_._//_/   \_. /"
echo "                   \___/"
echo ""
echo "Fury build tool for Scala, version $FURY_VERSION."
echo "This software is provided under the Apache 2.0 License."
echo "© Copyright 2018 Jon Pretty, Propensive Ltd."
echo ""
echo "This will install Fury on your computer."
echo ""
echo "Checking for dependencies..."

RED=$(printf '\e[31m')
GREEN=$(printf '\e[32m')
YELLOW=$(printf '\e[33m')
RESET=$(printf '\e[0m')

message() {
  echo " $GREEN*$RESET $1"
}

warn() {
  echo " $RED*$RESET $1"
}

fail() {
  warn "$1"
  exit 1
}

checkJava() {
  # We only want to capture stderr, stdout can be discarded.
  JAVA=$(java -version 2>&1 >/dev/null | head -n1 | grep -o '".*"' | sed 's/"//g')
  if [ -z "$JAVA" ]
  then
    warn "Did not find a local installed version of Java."
    warn "Please install Java, or make sure that the 'java' command is on the path."
    warn "Java is available here,"
    warn ""
    fail "  http://www.oracle.com/technetwork/java/javase/downloads/index.html"
  else
    message "Found Java version $JAVA."
  fi
}

completion() {
  java -cp '/build/dist/bundle/lib/*' fury.installer.Installer
}

checkJava && completion

exit 0
