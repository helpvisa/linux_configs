#!/bin/sh

IS_RECORDING="$(pgrep -l wf-recorder)"
FILENAME="$HOME/Videos/Screencasts/Screencast From $(date | sed 's/ /_/g;s/:/-/g').mp4"

if [ -z "$IS_RECORDING" ]; then
    notify-send "wf-recorder" "Recording started! Saving to $FILENAME" -t 500
    wf-recorder -g "$(slurp)" -f "$FILENAME"
else
    killall wf-recorder
    notify-send "wf-recorder" "Recording finished!"
fi
