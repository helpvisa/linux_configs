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

    ~/.local/bin/brotab activate "$TAB_ID"
    if [ $? -ne 0 ] || [ -z "$(~/.local/bin/brotab clients)" ]; then
        if printf "%s" "$SELECTION" | grep -q "\."; then
            URL="${SELECTION}"
        else
            URL="https://duckduckgo.com/?q=${SELECTION}"
        fi

        if ! pgrep firefox; then
            nohup firefox "${URL}" >/dev/null 2>&1 &
            echo "opening new firefox instance"
        else
            nohup firefox --new-tab "${URL}" >/dev/null 2>&1 &
        fi

        # this value may need changing depending on machine
        sleep 0.3
        TAB_NAME="$SELECTION"
    else
        TAB_NAME=$(printf "%s" "$SELECTION" | awk '{split($0,array,"\t"); print array[2]}')
    fi

    # now raise the window (wlr-specific)
    APP_DETAILS=$(wlrctl toplevel list | grep "$TAB_NAME")
    APP_ID="$(printf "%s" "$APP_DETAILS" | awk 'BEGIN {FS=": ";}{print $1}')"
    NAME="$(printf "%s" "$APP_DETAILS" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"
    wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
fi
