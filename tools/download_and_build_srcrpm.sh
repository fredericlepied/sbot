#!/bin/bash

set -eu

WSP=$1
URL=$2
DIR=$3

set -x

mkdir -p $WSP/$DIR
cd $WSP/$DIR
wget -q -r -np -nd -L -A '*.src.rpm' -A '*.cfg' $URL$DIR/
ls
rm -f child.cfg site-defaults.cfg

mock -r *.cfg --rebuild *.src.rpm

# download_and_build_srcrpm.sh ends here
