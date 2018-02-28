#!/bin/bash

set -eu

WSP=$1
URL=$2
DIR=$3

set -x

mkdir -p $WSP/$DIR
cd $WSP/$DIR
wget -q -r -np -nd -L -A '*.src.rpm' -A '*.cfg' -A 'copr.log' -A 'rpmbuild.log' $URL$DIR/
rm -f child.cfg site-defaults.cfg
ls *.cfg
ls *.src.rpm

# dlrn_download_srcrpm.sh ends here
