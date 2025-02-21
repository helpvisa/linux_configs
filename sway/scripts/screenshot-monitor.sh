#!/bin/sh

# use fuzzel to pick a monitor
MONITOR="$(wlr-randr --json | grep name | sed 's/ \|\"\|,//g' | cut -d':' -f2 | fuzzel -d)"
echo "$MONITOR"

grim -o "$MONITOR" - | wl-copy
# also save to disk
FILENAME="$HOME/Pictures/Screenshots/$MONITOR From $(date | sed 's/ /_/g;s/:/-/g').png"
wl-paste > "$FILENAME"
