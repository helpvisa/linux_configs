#!/bin/sh
# requires my fork of window calls (https://github.com/helpvisa/window-calls)

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
WINDOW=$(printf "%s" "$LIST" | \
             fzf --style=minimal \
                 --layout=reverse \
                 --margin 3% \
                 --prompt='raise window <> ')
# WINDOW=$(printf "%s" "$LIST" \
#     | BEMENU_BACKEND=curses bemenu -i -l 30 \
#     -H 25 \
#     --counter=always \
#     -p 'raise window <>' \
#     --fn 'Input Mono 12' \
#     --tb='#222222' \
#     --fb='#222222' \
#     --cb='#222222' \
#     --nb='#222222' \
#     --hb='#333333' \
#     --fbb='#222222' \
#     --sb='#222222' \
#     --ab='#222222' \
#     --scb='#222222' \
#     --tf='#222222' \
#     --tb='#d97f2b' \
#     --hf='#d97f2b')
# sed 's/^[^:]*://' -> non-greedy (replace up to first colon)
# sed 's/.*://' -> greedy (replace up to last colon)
SELECTION=$(printf "%s" "$WINDOW" | sed 's/.*://')
printf "%s" "$SELECTION"

if [ -z "$SELECTION" ]; then
    printf "%s" "no selection made!"
else
    gdbus call --session \
        --dest org.gnome.Shell \
        --object-path /org/gnome/Shell/Extensions/Windows \
        --method org.gnome.Shell.Extensions.Windows.Activate \
        "$(printf "%s" "$SELECTION")"
fi
