#!/bin/bash

CURDIR=$(cd $(dirname $0); pwd)

if [ ! -d $CURDIR/tools ]; then
    echo "Unable to find tools subdir. Aborting." 1>&2
    exit 1
fi

export PATH=$CURDIR/tools:$PATH

cd $CURDIR

if [ ! -r config.pl ]; then
    ln -s modules/*/config.pl . >& /dev/null
    if [ ! -r config.pl ]; then
        cp config.pl.ex config.pl
    fi
fi

if [ ! -r save.pl ]; then
    touch save.pl
fi

if [ "$1" = -d ]; then
    exec swipl -q -s config.pl -s load.pl
else
    exec swipl -q -s config.pl -s load.pl -g world:fact_loop
fi

# bot.sh ends here
