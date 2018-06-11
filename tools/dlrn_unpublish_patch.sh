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

PATCH=$(basename $PATCH)

if [ -r $PATCH ]; then
    git rm -f $PATCH
fi

git commit -m "removed $PATCH" *.spec $PATCH
git push

# dlrn_unpublish_patch.sh ends here
