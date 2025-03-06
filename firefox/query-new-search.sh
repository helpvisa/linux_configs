#!/bin/sh
# requires activate-window-by-title and window-calls extensions

SELECTION=$(printf "%s" "" \
    | fzf --bind=enter:replace-query+print-query \
    --style=minimal \
    --layout=reverse --margin 3% --prompt='search the web: ')
# SELECTION=$(BEMENU_BACKEND=curses bemenu -i -p 'raise window:')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    if printf "%s" "$SELECTION" | grep -q "\."; then
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

        # keep checking for our new tab
        SELECTION=$(printf "%s" "$SELECTION" | sed 's/\*//g')
        CHECK=1
        while [ -z "$WINDOW" ] && [ "25" -gt "$CHECK" ]; do
            # (gnome-specific)
            LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
                    --object-path /org/gnome/Shell/Extensions/Windows \
                    --method org.gnome.Shell.Extensions.Windows.List \
                    | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
            LIST=$(printf "%s" "$LIST_RAW" \
                | jq -r '.[] | select( .title != null ) | "\(.wm_class): \(.title) :\(.id)"' \
                | sed 's/\*//g')
            WINDOW=$(printf "%s" "$LIST" \
                | grep -i "$SELECTION" \
                | sed 's/.*://')
            # increment check counter
            CHECK=$(echo "$CHECK 1 + p" | dc)
            printf "%d\n" "$CHECK"
            sleep 0.2
        done

        # make sure only one line exists
        WINDOW=$(printf "%s" "$WINDOW" | head -n1)

        # finally raise the acquired window ID
        gdbus call --session \
            --dest org.gnome.Shell \
            --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
            --method de.lucaswerkmeister.ActivateWindowByTitle.activateById \
            "$(printf "%s" "$WINDOW")"
    fi
fi
