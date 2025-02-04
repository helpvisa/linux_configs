#!/bin/sh

# maim -s | xclip -selection clipboard -t image/png
grim -g "$(slurp)" - | wl-copy
