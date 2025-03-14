#!/bin/sh
# requires brotab (pip install --user brotab)
# requires activate-window-by-title

LIST=$(~/.local/bin/brotab list)
# SELECTION=$(printf "%s" "$LIST" | fzf --style=minimal --layout=reverse --margin 5% --prompt='activate tab: ')
# SELECTION=$(printf "%s" "$LIST" | fuzzel -d --placeholder='activate browser tab')
SELECTION=$(printf "%s" "$LIST" \
    | BEMENU_BACKEND=wayland bemenu -i -l 30 -f \
    -H 25 \
    --counter=always \
    -p 'activate tab <>' \
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

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')
    TAB_NAME=$(printf "%s" "$SELECTION" \
        | sed 's/^[^\t]*\t//' \
        | sed 's/\t.*//')

    ~/.local/bin/brotab activate "$TAB_ID"
    if [ $? -ne 0 ] || [ -z "$(~/.local/bin/brotab clients)" ]; then
        if printf "%s" "$SELECTION" | grep -q "\.ca\|\.com\|\.org\|\.net\|\.io"; then
            URL="${SELECTION}"
            SELECTION=$(printf "%s" "$SELECTION" | cut -d'.' -f1)
        else
            URL="https://duckduckgo.com/?q=${SELECTION}"
        fi

        if ! pgrep firefox; then
            nohup firefox "${URL}" >/dev/null 2>&1 &
            echo "opening new firefox instance"
            sleep 1
        else
            nohup firefox --new-tab "${URL}" >/dev/null 2>&1 &
        fi
    fi

    # keep checking for our new tab
    CHECKS=1
    while [ -z "$NAME" ] && [ "25" -gt "$CHECKS" ]; do
        # (wlr-specific)
        APP_DETAILS=$(wlrctl toplevel list | grep "$TAB_NAME")
        APP_ID="$(printf "%s" "$APP_DETAILS" \
            | awk 'BEGIN {FS=": ";}{print $1}')"
        NAME="$(printf "%s" "$APP_DETAILS" \
            | sed 's/^[^:\ ]*:\ //')"
        # increment check counter
        CHECKS=$(echo "$CHECKS 1 + p" | dc)
        printf "%d\n" "$CHECKS"
        sleep 0.2
    done
    printf "%s\n" "$NAME"
    wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
fi
