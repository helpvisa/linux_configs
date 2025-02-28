#!/bin/sh
# requires brotab (pip install --user brotab)
# requires activate-window-by-title

LIST=$(~/.local/bin/brotab list)
# SELECTION=$(printf "%s" "$LIST" | fzf --style=minimal --layout=reverse --margin 5% --prompt='activate tab: ')
SELECTION=$(printf "%s" "$LIST" | fuzzel -d --placeholder='activate browser tab')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')

    ~/.local/bin/brotab activate "$TAB_ID"
    if [ $? -ne 0 ]; then
        firefox --new-tab "https://duckduckgo.com/?q=${SELECTION}"
        # this value may need changing depending on machine
        sleep 0.2
        TAB_NAME="$SELECTION"
    else
        TAB_NAME=$(printf "%s" "$SELECTION" | awk '{split($0,array,"\t"); print array[2]}')
    fi

    # now raise the window (wlr-specific)
    printf "%s\n" "$TAB_NAME"
    APP_DETAILS=$(wlrctl toplevel list | grep "$TAB_NAME")
    printf "%s\n" "$APP_DETAILS"
    APP_ID="$(printf "%s" "$APP_DETAILS" | awk 'BEGIN {FS=": ";}{print $1}')"
    # printf "%s\n" "$APP_ID"
    NAME="$(printf "%s" "$APP_DETAILS" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"
    # printf "%s\n" "$NAME"
    wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
fi
