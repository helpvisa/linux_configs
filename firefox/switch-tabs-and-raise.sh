#!/bin/sh
# requires brotab (pip install --user brotab)
# requires activate-window-by-title

LIST=$(brotab list)
SELECTION=$(printf "%s" "$LIST" | fzf --bind=enter:replace-query+print-query --style=minimal --layout=reverse --margin 5% --prompt='activate tab: ')
# SELECTION=$(printf "%s" "$LIST" | BEMENU_BACKEND=curses bemenu -i -p 'raise window:')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')

    brotab activate "$TAB_ID"
    if [ $? -ne 0 ]; then
        firefox --new-tab "https://duckduckgo.com/?q=${SELECTION}"
        # this delay may need to be modified depending on computer + connection
        sleep 0.5
    fi

    # now raise the window (gnome-specific
    LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
            --object-path /org/gnome/Shell/Extensions/Windows \
            --method org.gnome.Shell.Extensions.Windows.List \
            | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
    LIST=$(printf "%s" "$LIST_RAW" | jq -r '.[] | select( .title != null ) | "\(.wm_class): \(.title)"')
    WINDOW_NAME=$(printf "%s" "$LIST" | grep -i "$SELECTION" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
        --method de.lucaswerkmeister.ActivateWindowByTitle.activateBySubstring \
        "$(printf "%s" "$WINDOW_NAME")"
fi
