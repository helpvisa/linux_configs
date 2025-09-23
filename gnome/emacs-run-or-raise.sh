#!/bin/sh
# requires my fork of window calls (https://github.com/helpvisa/window-calls)

# head and tail strip brackets + quotes at beginning and end that ruin the json
# WindowsExt.List has a nasty tendency to escape quotes randomly
# sed can fix this behaviour
LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
           --object-path /org/gnome/Shell/Extensions/Windows \
           --method org.gnome.Shell.Extensions.Windows.List |
             head -c -4 | tail -c +3 | sed 's/\\"/"/g')
SELECTION=$(printf "%s" "$LIST_RAW" \ |
            jq -r '.[] | select( .wm_class == "emacs" ) | .id' |
            sort |
            head -n 1)
printf "%s" "$SELECTION"

if [ -z "$SELECTION" ]; then
    printf "%s\n" "no emacs instance!"
    emacsclient -c
else
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/Windows \
        --method org.gnome.Shell.Extensions.Windows.Activate \
        "$(printf "%s" "$SELECTION")"
fi
