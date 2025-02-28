#!/bin/sh

# WINDOW="$(wlrctl toplevel list | fuzzel -d --placeholder='search through windows...')"
WINDOW=$(wlrctl toplevel list \
    | BEMENU_BACKEND=wayland bemenu -i -l 30 -f \
    -H 24 \
    --counter=always \
    -p 'raise window <>' \
    --fn 'Input Mono 12' \
    --tb='#222222' \
    --fb='#222222' \
    --cb='#222222' \
    --nb='#222222' \
    --hb='#333333' \
    --fbb='#222222' \
    --sb='#222222' \
    --ab='#222222' \
    --scb='#222222' \
    --tf='#222222' \
    --tb='#d97f2b' \
    --hf='#d97f2b')

APP_ID="$(printf "%s" "$WINDOW" | awk 'BEGIN {FS=": ";}{print $1}')"
NAME="$(printf "%s" "$WINDOW" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"

wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
# we echo to trim whitespace
echo "app_id:${APP_ID}" "title:${NAME}"
