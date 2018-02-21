#!/bin/bash

set -eu

WS="$1"
VER="$2"
REL="$3"

set -x

mkdir -p $WS
cd $WS

sed -i -e "s/Version:\(\s*\)\(.*\)/Version:\1$VER/" -e "s/Release:\(\s*[0-9.]*\)/Release: $REL/" *.spec

git diff

# rpmsec_set_vr.sh ends here
