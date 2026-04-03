#!/bin/sh
# requires window-calls extension

LIST_RAW=$(dbus-send --session \
                     --print-reply=literal \
                     --dest=org.gnome.Shell \
                     /org/gnome/Shell/Extensions/Windows \
                     org.gnome.Shell.Extensions.Windows.List)
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
