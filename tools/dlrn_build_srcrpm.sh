#!/bin/bash

set -eu

DIR=$1

set -x

cd $DIR

mock -r $(ls ../*.cfg *.cfg) --resultdir=$PWD --rebuild $(ls *.src.rpm|head -1)

# dlrn_build_srcrpm.sh ends here
