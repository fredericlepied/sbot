#!/bin/bash

set -eu

WSP=$1
DIR=$2

set -x

cd $WSP/$DIR

mock -r $(ls ../*.cfg *.cfg) --rebuild *.src.rpm

# dlrn_build_srcrpm.sh ends here
