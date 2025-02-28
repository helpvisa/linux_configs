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

    # now raise the window (wlr-specific)
    TAB_NAME=$(printf "%s" "$SELECTION" | awk '{split($0,array,"\t"); print array[2]}')
    APP_DETAILS=$(wlrctl toplevel list | grep "$(printf "%s" "$TAB_NAME")")
    APP_ID="$(printf "%s" "$APP_DETAILS" | awk 'BEGIN {FS=": ";}{print $1}')"
    NAME="$(printf "%s" "$APP_DETAILS" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')"
    wlrctl toplevel focus "app_id:${APP_ID}" "title:${NAME}" "state:inactive"
fi
