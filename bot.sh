#!/bin/bash

CURDIR=$(cd $(dirname $0); pwd)

if [ ! -d $CURDIR/tools ]; then
    echo "Unable to find tools subdir. Aborting." 1>&2
    exit 1
fi

export PATH=$CURDIR/tools:$PATH

exec swipl -q -s load.pl -g get_facts

# bot.sh ends here
