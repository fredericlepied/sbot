#!/bin/bash

set -e

USER="$1"

set -u

WS="$2"
PKG="$3"

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
    git stash
    git checkout master
    git pull
fi

# fedora_get_distgit.sh ends here
