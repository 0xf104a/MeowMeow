#!/bin/sh

# Boot script for docker

echo "=> DEBUG: $0 $1"
echo "=> Booting MeowMeow webserver"

if [[ $1 == "shell" ]]; then
   echo "=> Entering pre-test shell"
   /bin/sh
fi

/MeowMeow/bin/MeowMeow daemon
ruby "/testroot/SimpleTest/src/test.rb" /tests
