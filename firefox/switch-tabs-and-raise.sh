#!/bin/sh
# requires brotab (pipx install brotab)
# requires activate-window-by-title and window-calls extensions

LIST=$(brotab list)
SELECTION=$(printf "%s" "$LIST" \
    | fzf --bind=enter:replace-query+print-query \
    --style=minimal --layout=reverse --margin 3% \
    --prompt='activate tab: ')
# SELECTION=$(printf "%s" "$LIST" | BEMENU_BACKEND=curses bemenu -i -p 'raise window:')

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no selection made!"
else
    TAB_ID=$(printf "%s" "$SELECTION" | awk '{print $1}')
    # remove tab id and url, leaving only title
    TAB_TITLE=$(printf "%s" "$SELECTION" \
        | sed 's/^[^\t]*\t//' \
        | sed 's/\t.*//')

    brotab activate "$TAB_ID" 2>/dev/null
    if [ $? -ne 0 ] || [ -z "$(brotab clients)" ]; then
        if printf "%s" "$SELECTION" | grep -q "\."; then
            URL="${SELECTION}"
            SELECTION="$(printf "%s" "$SELECTION" | cut -d'.' -f1)"
        else
            URL="https://duckduckgo.com/?q=${SELECTION}"
        fi

        # check if firefox is already running
        if ! pgrep firefox; then
            nohup firefox "${URL}" >/dev/null 2>&1 &
            echo "opening new firefox instance"
            sleep 1
        else
            nohup firefox --new-tab "${URL}" >/dev/null 2>&1 &
        fi
    fi

    # keep checking for our new tab or window
    while [ -z "$WINDOW" ]; do
        # (gnome-specific)
        LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
                --object-path /org/gnome/Shell/Extensions/Windows \
                --method org.gnome.Shell.Extensions.Windows.List \
                | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
        LIST=$(printf "%s" "$LIST_RAW" \
            | jq -r '.[] | select( .title != null ) | "\(.wm_class): \(.title) :\(.id)"')
        WINDOW=$(printf "%s" "$LIST" \
            | grep -i "$TAB_TITLE" \
            | sed 's/.*://')
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
