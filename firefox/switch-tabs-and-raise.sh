#!/bin/sh
# requires brotab (pip install --user brotab)
# requires activate-window-by-title

LIST=$(brotab list)
SELECTION=$(printf "%s" "$LIST" | fzf --bind=enter:replace-query+print-query --style=minimal --layout=reverse --margin 3% --prompt='activate tab: ')
# SELECTION=$(printf "%s" "$LIST" | BEMENU_BACKEND=curses bemenu -i -p 'raise window:')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')

    brotab activate "$TAB_ID" 2>/dev/null
    if [ $? -ne 0 ] || [ -z "$(brotab clients)" ]; then
        if printf "%s" "$SELECTION" | grep -q "\."; then
            URL="${SELECTION}"
            SELECTION="$(printf "%s" "$SELECTION" | cut -d'.' -f1)"
        else
            URL="https://duckduckgo.com/?q=${SELECTION}"
        fi

        if ! pgrep firefox; then
            nohup firefox "${URL}" >/dev/null 2>&1 &
            echo "opening new firefox instance"
            # sleep to make sure firefox opens
            sleep 1
        else
            nohup firefox --new-tab "${URL}" >/dev/null 2>&1 &
        fi

        # this delay may need to be modified depending on computer + connection
        sleep 0.5
        # (gnome-specific)
        LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
                --object-path /org/gnome/Shell/Extensions/Windows \
                --method org.gnome.Shell.Extensions.Windows.List \
                | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
        LIST=$(printf "%s" "$LIST_RAW" | jq -r '.[] | select( .title != null ) | "\(.wm_class): \(.title)"')
        WINDOW_NAME=$(printf "%s" "$LIST" | grep -i "$SELECTION" | awk '{split($0,f,": "); sub(/^([^: ]+: )/,"",$0); print $0}')
    else
        WINDOW_NAME=$(printf "%s" "$SELECTION" | awk '{split($0,array,"\t"); print array[2]}')
        if [ "$WINDOW_NAME" = "New Tab" ]; then
            WINDOW_NAME="Mozilla Firefox"
        fi
    fi

    if [ -z "$WINDOW_NAME" ]; then
        WINDOW_NAME="Mozilla Firefox"
    fi
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
        --method de.lucaswerkmeister.ActivateWindowByTitle.activateBySubstring \
        "$(printf "%s" "$WINDOW_NAME")"
fi
