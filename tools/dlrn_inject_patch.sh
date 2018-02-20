#!/bin/bash

set -eu

DIR=$1
PATCH=$2

set -x

mkdir -p $$DIR
cd $DIR

rpm --define "_topdir $PWD" -i *.src.rpm
cp $PATCH SOURCES/
add_patch.py $(basename $PATCH) SPECS/*.spec "# temporary patches for DLRN"
rpmbuild --define "_topdir $PWD" -bs SPECS/*.spec

# dlrn_inject_patch.sh ends here
