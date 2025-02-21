#!/bin/sh

# maim -s | xclip -selection clipboard -t image/png
# grim -g "$(slurp)" - | wl-copy
wayfreeze --hide-cursor & PID=$!; sleep .1; grim -g "$(slurp)" - | wl-copy; kill $PID
# also save to disk
FILENAME="$HOME/Pictures/Screenshots/Screenshot From $(date | sed 's/ /_/g;s/:/-/g').png"
wl-paste > "$FILENAME"
