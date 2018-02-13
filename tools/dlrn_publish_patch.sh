#!/bin/bash

set -eu

DIR=$1
PATCH=$2
BRANCH=$3

set -x

cd $DIR

git checkout $BRANCH
git pull
cp $PATCH .
add_patch.py $(basename $PATCH) *.spec "# temporary patches for DLRN"
git add $(basename $PATCH)
git commit -m "Added $(basename $PATCH)" *.spec $(basename $PATCH)
git push

# dlrn_publish_patch.sh ends here
