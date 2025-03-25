#!/bin/sh
# requires actiavte-window-by-title and window-calls extension

# head and tail strip brackets + quotes at beginning and end that ruin the json
# WindowsExt.List has a nasty tendency to escape quotes randomly
# sed can fix this behaviour
LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/Windows \
        --method org.gnome.Shell.Extensions.Windows.List \
        | head -c -4 | tail -c +3 | sed 's/\\"/"/g')
FOCUS=$(printf "%s" "$LIST_RAW" \
    | jq -r '.[] | select( .focus == true ) | "\(.id)"')

if [ -z "$FOCUS" ]; then
    printf "%s" "no window focused!"
else
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/Windows \
        --method org.gnome.Shell.Extensions.Windows.MoveResize \
        "$(printf "%s" "$FOCUS")" 2560 0 1440 1280
fi
