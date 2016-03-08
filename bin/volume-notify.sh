#!/usr/bin/env bash

set -e

NOTIFY_ID_FILE=~/.cache/audio-notify-id

replace_id="$(cat $NOTIFY_ID_FILE || echo '')"
if [[ -z $replace_id ]]; then
    replace_id=0
fi

function vol-icon {
    if [[ $1 -gt 66 ]]; then
        echo audio-volume-high
    elif [[ $1 -gt 33 ]]; then
        echo audio-volume-medium
    else
        echo audio-volume-low
    fi
}

value=0
icon=speaker

case $1 in
    "vol" )
        value=$2
        icon=$(vol-icon $value)
        ;;
    "mute" )
        value=0
        icon=audio-volume-muted
        ;;
esac

send-notify.sh -p -r $replace_id "Volume" -i "$icon" -h int:value:$value > $NOTIFY_ID_FILE
