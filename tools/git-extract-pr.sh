#!/bin/bash

set -eu

PR=$1

MASTER=$(basename $(cat .git/refs/remotes/origin/HEAD | cut -d' ' -f2))

set -x

git checkout $MASTER
git pull
git branch -D pr-$PR || :
git fetch origin pull/$PR/head:pr-$PR
git checkout pr-$PR
git rebase origin/$MASTER
git diff -r $MASTER > pr-$PR.patch

# git-extract-pr.sh ends here
