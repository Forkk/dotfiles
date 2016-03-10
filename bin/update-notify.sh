#!/usr/bin/env bash

set -e

NOTIFY_ID="$1"

NOTIFY_ID_FILE=~/.cache/"$NOTIFY_ID-notify-id"

(
    flock -n 9 || exit 1

    replace_args=
    replace_id="$(cat $NOTIFY_ID_FILE 2>/dev/null || echo '')"
    if [[ ! -z $replace_id ]]; then
        replace_args="--replace $replace_id"
    fi

    send-notify.sh -p $replace_args "${@:2}" > $NOTIFY_ID_FILE
) 9>$NOTIFY_ID_FILE.lock
