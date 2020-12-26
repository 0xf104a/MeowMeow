#!/bin/sh

# Boot script for docker

echo "=> DEBUG: $0 $1"
echo "=> Booting MeowMeow webserver"
/MeowMeow/bin/MeowMeow foreground
