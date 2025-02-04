#!/bin/sh

# start by screenshotting the screen
grim - > ~/Scripts/sway/screenshot.jpg
# now blur it
convert ~/Scripts/sway/screenshot.jpg -blur 0x16 ~/Scripts/swaylock/screenshot.jpg
# now lock the screen
swaylock -f -i ~/Scripts/sway/screenshot.jpg
