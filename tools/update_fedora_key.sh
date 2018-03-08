#!/bin/bash

set -eu

DIR=$1
KEY=$2
PORT=3300
SERVER=fedora@38.145.33.116

set -x

cd $DIR

if [ ! -d fedora-repos ]; then
    git clone https://pagure.io/fedora-repos.git
    cd fedora-repos
else
    cd fedora-repos
    git pull
fi

scp -P $PORT RPM-GPG-KEY-fedora-${KEY}-primary $SERVER:/tmp/
ssh -p $PORT $SERVER sudo cp /tmp/RPM-GPG-KEY-fedora-${KEY}-primary /etc/pki/mock/

# update_fedora_key.sh ends here
