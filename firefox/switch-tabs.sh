#!/bin/sh
# requires brotab (pip install --user brotab)

LIST=$(brotab list)
SELECTION=$(printf "%s" "$LIST" | fzf --style=minimal --layout=reverse --margin 5% --prompt='activate tab: ')
TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')
brotab activate "$TAB_ID"
