#!/usr/bin/env bash
docker build -t test.meow.meow -f tests/Dockerfile .
if [[ $? != 0 ]]; then
   echo "Failed to build test image"
   exit -1
fi
docker run test.meow.meow
