#!/bin/bash

set -e

USER="$1"

set -u

WS="$2"
PKG="$3"
VER="$4"

set -x

mkdir -p $WS
cd $WS

if [ -n "$USER" ]; then
    OPT="--user $USER"
else
    OPT=
fi

if [ ! -d $PKG ]; then
    fedpkg $OPT clone $PKG
    cd $PKG
else
    cd $PKG
    git checkout master
    git pull
fi

sed -i -e "s/Version: \(.*\)/Version: $VER/" -e "s/Release: \([0-9.]*\)/Release: 1/" *.spec

git diff

spectool -S -g *.spec

fedpkg mockbuild

# fedora_update_pkg.sh ends here
