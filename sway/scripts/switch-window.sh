#!/bin/sh

WINDOW="$(wlrctl toplevel list | fuzzel -d --placeholder='search through windows...')"
echo "$WINDOW"
APP_ID="$(printf "%s" "$WINDOW" | awk 'BEGIN {FS=": ";}{print $1}')"
NAME="$(printf "%s" "$WINDOW" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"

wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
# we echo to trim whitespace
echo "app_id:${APP_ID}" "title:${NAME}"
