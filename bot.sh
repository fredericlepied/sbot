#!/bin/bash

CURDIR=$(cd $(dirname $0); pwd)

if [ ! -d $CURDIR/tools ]; then
    echo "Unable to find tools subdir. Aborting." 1>&2
    exit 1
fi

export PATH=$CURDIR/tools:$PATH

if [ "$1" = -d ]; then
    exec swipl -q -s load.pl -g get_facts
else
    exec swipl -q -s load.pl -s irc.pl -g run
fi

# bot.sh ends here
