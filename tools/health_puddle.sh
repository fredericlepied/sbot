#!/bin/bash

function linckchecker {
    URL=$1
    CONTENT=$(curl -IL $1 2>/dev/null)

    echo $CONTENT | grep '404 Not Found' > /dev/null
    if [[ $? -eq 0 ]]; then
        return 1
    else
        return 0
    fi
}

function check_osp_status {
    linckchecker ${PUDDLE_URL}/status.txt

    if [[ $? -eq 1 ]]; then
        return 1
    fi

    STATUS=$(curl -L ${PUDDLE_URL}/status.txt 2>/dev/null)
    if [[ ! $STATUS == "COMPLETE" ]]; then
        echo "status.txt: $STATUS"
        exit 1
    else
        echo "status.txt: $STATUS"
    fi
}

function check_dlrn_data {
    linckchecker ${PUDDLE_URL}/import_data/commit.yaml

    if [[ $? -eq 1 ]]; then
        echo "import_data/commit.yaml: file is missing"
        exit 1
    else
        echo "import_data/commit.yaml: OK"
    fi
}

function check_containers {
    linckchecker ${PUDDLE_URL}/overcloud_container_image_prepare.yaml

    if [[ $? -eq 1 ]]; then
        echo "overcloud_container_image_prepare.yaml: file is missing"
        exit 1
    else
        echo "overcloud_container_image_prepare.yaml: OK"
    fi
}

function check_embargo {
    linckchecker ${PUDDLE_URL}/EMBARGOED

    if [[ $? -eq 1 ]]; then
        echo "EMBARGOED: Not under embargo"
    else
        echo "EMBARGOED: Puddle under embargo"
        exit 1
    fi
}

function check_rhel_status {
    linckchecker ${PUDDLE_URL}/STATUS

    if [[ $? -eq 1 ]]; then
        return 1
    fi

    STATUS=$(curl -L ${PUDDLE_URL}/STATUS 2>/dev/null)
    if [[ ! $STATUS == "FINISHED" ]]; then
        echo "status.txt: $STATUS"
        exit 1
    else
        echo "status.txt: $STATUS"
    fi
}

PUDDLE_URL=$1

if [[ $PUDDLE_URL = *"OpenStack"*   ]]; then
    OSP_VERSION=$(echo ${PUDDLE_URL} | sed 's#.*/\(.*\).0-RHEL-7/.*$#\1#')
    PUDDLE_URL="${PUDDLE_URL}/latest"
    check_osp_status
    check_embargo

    if [[ $OSP_VERSION -ge 11 ]]; then
        check_dlrn_data
    fi

    if [[ $OSP_VERSION -ge 12 ]]; then
        check_containers
    fi
else
    check_rhel_status
fi
