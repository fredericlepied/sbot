#!/bin/bash

set -eu

WS="$1"
CMD="$2"

set -x

mkdir -p $WS
cd $WS

spectool -S -g *.spec

fedpkg $CMD

# fedora_fedpkg.sh ends here
