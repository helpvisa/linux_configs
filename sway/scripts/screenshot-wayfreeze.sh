#!/bin/sh

# maim -s | xclip -selection clipboard -t image/png
# grim -g "$(slurp)" - | wl-copy
./wayfreeze & PID=$!; sleep .1; grim -g "$(slurp)" - | wl-copy; kill $PID
