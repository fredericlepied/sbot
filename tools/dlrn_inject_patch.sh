#!/bin/bash

set -eu

WSP=$1
DIR=$2
PATCH=$3

set -x

mkdir -p $WSP/$DIR
cd $WSP/$DIR

rpm --define "_topdir $PWD" -i *.src.rpm
cp $PATCH SOURCES/
add_patch.py $(basename $PATCH) SPECS/*.spec "# temporary patches for DLRN"
rpmbuild --define "_topdir $PWD" -bs SPECS/*.spec

# dlrn_inject_patch.sh ends here
