#!/bin/sh
# requires jq,
# activate-window-by-title extension,
# and window-calls-extended extension

# head and tail strip brackets + quotes at beginning and end that ruin the json
# WindowsExt.List has a nasty tendency to escape quotes randomly
# sed can fix this behaviour
LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/WindowsExt \
        --method org.gnome.Shell.Extensions.WindowsExt.List \
        | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
LIST=$(printf "%s" "$LIST_RAW" | jq -r '.[].title')

# use picker to select window; must be GNOME compatible so no fuzzel :(
WINDOW=$(printf "%s" "$LIST" | fzf)
gdbus call --session \
    --dest org.gnome.Shell \
    --object-path /de/lucaswerkmeister/ActivateWindowByTitle \
    --method de.lucaswerkmeister.ActivateWindowByTitle.activateBySubstring \
    "$(printf "%s" "$WINDOW")"
