#!/bin/bash

CURDIR=$(cd $(dirname $0); pwd)

if [ ! -d $CURDIR/tools ]; then
    echo "Unable to find tools subdir. Aborting." 1>&2
    exit 1
fi

export PATH=$CURDIR/tools:$PATH

if [ ! -r config.pl ]; then
    cp config.pl.ex config.pl
fi

if [ "$1" = -d ]; then
    exec swipl -q -s config.pl -s load.pl
else
    exec swipl -q -s config.pl -s load.pl -g world:fact_loop
fi

# bot.sh ends here
