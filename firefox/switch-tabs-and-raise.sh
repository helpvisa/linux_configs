#!/bin/sh
# requires brotab (pip install --user brotab)
# requires activate-window-by-title

LIST=$(brotab list)
# SELECTION=$(printf "%s" "$LIST" | fzf --style=minimal --layout=reverse --margin 5% --prompt='activate tab: ')
SELECTION=$(printf "%s" "$LIST" | BEMENU_BACKEND=curses bemenu -i -p 'raise window:')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')

    brotab activate "$TAB_ID"
    if [ $? -ne 0 ]; then
        firefox --new-tab "https://duckduckgo.com/?q=${SELECTION}"
    fi

    # now raise the window (gnome-specific
    WINDOW_NAME=$(printf "%s" "$SELECTION" | awk '{split($0,array,"\t"); print array[2]}')
    printf "%s" "$WINDOW_NAME"
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
        --method de.lucaswerkmeister.ActivateWindowByTitle.activateBySubstring \
        "$(printf "%s" "$WINDOW_NAME")"
fi
