#!/bin/bash

set -eu

DIR=$1

set -x

cd $DIR

mock -r $(ls ../*.cfg *.cfg) --rebuild *.src.rpm

# dlrn_build_srcrpm.sh ends here
