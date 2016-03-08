#!/usr/bin/env bash

set -e

NOTIFY_ID_FILE=~/.cache/audio-notify-id

NOTIFY_ARGS=(--session
             --dest org.freedesktop.Notifications
             --object-path /org/freedesktop/Notifications)

# We don't use notify-send since it doesn't support replacing existing notifications.
function notify_call {
    gdbus call "${NOTIFY_ARGS[@]}"  --method org.freedesktop.Notifications.Notify \
          "AudioSwitcher" "$4" "$3" "$1" "$2" \
          [] "[]" "int32 -1" | \
        sed 's/(uint32 \([0-9]\+\),)/\1/g'
}

replace_id="$(cat $NOTIFY_ID_FILE || echo '')"
if [[ -z $replace_id ]]; then
    replace_id=0
fi

notify_call "$1" "$2" "$3" "$replace_id" > $NOTIFY_ID_FILE
