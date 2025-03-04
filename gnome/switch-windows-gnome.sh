#!/bin/sh
# requires actiavte-window-by-title and window-calls extension

# head and tail strip brackets + quotes at beginning and end that ruin the json
# WindowsExt.List has a nasty tendency to escape quotes randomly
# sed can fix this behaviour
LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/Windows \
        --method org.gnome.Shell.Extensions.Windows.List \
        | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
LIST=$(printf "%s" "$LIST_RAW" \
    | jq -r '.[] | select( .title != null ) | "\(.wm_class): \(.title) :\(.id)"')

# use picker to select window; must be GNOME compatible so no fuzzel :(
# WINDOW=$(printf "%s" "$LIST" \
#     | BEMENU_BACKEND=curses bemenu -i -p 'raise window:')
WINDOW=$(printf "%s" "$LIST" \
    | fzf --style=minimal --layout=reverse --margin 3% --prompt='raise window: ')
# sed 's/^[^:]*://' -> non-greedy (replace up to first colon)
# sed 's/.*://' -> greedy (replace up to last colon)
SELECTION=$(printf "%s" "$WINDOW" | sed 's/.*://')
printf "%s" "$SELECTION"

if [ -z "$SELECTION" ]; then
    printf "%s" "no selection made!"
else
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
        --method de.lucaswerkmeister.ActivateWindowByTitle.activateById \
        "$(printf "%s" "$SELECTION")"
fi
