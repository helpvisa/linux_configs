#!/bin/sh

# make sure we are using the correct ydotool socket
export YDOTOOL_SOCKET="/tmp/ydotool_socket"

# save the current cursor state
STATE_LOCATION="/tmp/mmb_toggle_state"
STATE=$(cat "$STATE_LOCATION")

if printf "%s" "$STATE" | grep -q "on"; then
    echo "off" > "$STATE_LOCATION"
    ydotool click 0x82
else
    echo "on" > "$STATE_LOCATION"
    ydotool click 0x42
fi
