#!/bin/bash

set -eu

DIR=$1
PATCH=$2
BRANCH=$3

set -x

cd $DIR

git checkout $BRANCH
git pull
remove_patch.py $PATCH *.spec
git rm -f $(basename $PATCH)
git commit -m "removed $PATCH" *.spec $PATCH
git push

# dlrn_unpublish_patch.sh ends here
