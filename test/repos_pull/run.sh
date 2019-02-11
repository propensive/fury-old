#!/bin/bash
set -e
set -x

source $TESTROOTDIR/tests_lib

mkdir -p repo1
(
    cd repo1
    mkdir src
    git init
    touch src/version1 && git add -A && git commit -m"."
    
)

mkdir -p fury_project
(
    cd fury_project
    fury layer init
    fury project add -n webpage
    fury module add -n hello_world
    fury repo add -u https://github.com/propensive/base.git -n base
    fury import add -i base:2.12.6
    fury module update -c scala/compiler
    fury repo add -u $(realpath ../repo1) -n repo1 --version master
    fury source add -d repo1:src
)

(
    cd repo1
    touch src/version2 && git add -A && git commit -m"."
)

(
    cd fury_project
    fury repo pull --all
    fury
    git commit -am"."
)

(
    cd repo1
    touch src/version3 && git add -A && git commit -m"."
)

git clone fury_project fury_project_clone 2> /dev/null

(
    cd fury_project_clone
    fury
    ! find .fury/sources -name version1 -exec false \;
    ! find .fury/sources -name version2 -exec false \;
    find .fury/sources -name version2 -exec false \;
)
