#!/bin/sh

# Boot script for docker

echo "=> DEBUG: $0 $1"
echo "=> Booting MeowMeow webserver tests"

if [[ $# -ge 1 ]]; then
 if [[ $1 == "shell" ]]; then
    echo "=> Entering pre-test shell"
    /bin/sh
 fi
fi

export TESTROOT="/tests/"

/MeowMeow/bin/MeowMeow daemon
ruby "/testroot/SimpleTest/src/test.rb" /tests
