#!/bin/bash

set -eu

URL=$1

cd $(dirname $0)/..
mkdir -p modules
cd modules

MOD=$(basename $URL)

if [ ! -d $MOD ]; then
    git clone $URL
fi

# extract_module.sh ends here
