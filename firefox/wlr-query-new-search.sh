#!/bin/sh
# requires activate-window-by-title

SELECTION=$(printf "%s" "" \
    | BEMENU_BACKEND=wayland bemenu -i -l 30 \
    -H 25 \
    --counter=always \
    -p 'search the web <>' \
    --fn 'Input Mono 12' \
    --tb='#3b546a' \
    --fb='#3b546a' \
    --cb='#3b546a' \
    --nb='#3b546a' \
    --hb='#4c657b' \
    --fbb='#3b546a' \
    --sb='#3b546a' \
    --ab='#3b546a' \
    --scb='#3b546a' \
    --tf='#3b546a' \
    --tb='#d97f2b' \
    --hf='#d97f2b')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    if printf "%s" "$SELECTION" | grep -q "\."; then
        URL="${SELECTION}"
    else
        URL="https://duckduckgo.com/?q=${SELECTION}"
    fi

    if [ -z "$(pgrep firefox)" ]; then
        nohup firefox "${URL}" >/dev/null 2>&1 &
        echo "opening new firefox instance"
    else
        nohup firefox --new-tab "${URL}" >/dev/null 2>&1 &
        echo "using existing firefox instance"

        # this delay may need to be modified depending on computer + connection
        sleep 0.3
        # (wlr-specific)
        APP_DETAILS=$(wlrctl toplevel list | grep "$SELECTION")
        APP_ID="$(printf "%s" "$APP_DETAILS" | awk 'BEGIN {FS=": ";}{print $1}')"
        NAME="$(printf "%s" "$APP_DETAILS" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"
        wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
    fi
fi
