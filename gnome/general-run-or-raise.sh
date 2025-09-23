#!/bin/sh
# requires Window Calls GNOME extension

if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Please specify a window class name to check for and a program to launch."
    exit
else
    LIST_RAW=$(gdbus call --session --dest org.gnome.Shell \
               --object-path /org/gnome/Shell/Extensions/Windows \
               --method org.gnome.Shell.Extensions.Windows.List |
                 head -c -4 | tail -c +3 | sed 's/\\"/"/g')
    SELECTION=$(printf "%s" "$LIST_RAW" \ |
                jq -r ".[] | select( .wm_class == \"$1\" ) | .id" |
                sort |
                head -n 1)
    printf "%s" "$SELECTION"

    if [ -z "$SELECTION" ]; then
        printf "%s\n" "no program!"
        eval "$2"
    else
        gdbus call --session \
              --dest org.gnome.Shell \
              --object-path /org/gnome/Shell/Extensions/Windows \
              --method org.gnome.Shell.Extensions.Windows.Activate \
              "$(printf "%s" "$SELECTION")"
    fi
fi
