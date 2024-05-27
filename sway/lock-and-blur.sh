#!/bin/sh

# start by screenshotting the screen
grim - > ~/Scripts/swaylock/screenshot.jpg
# now blur it
convert ~/Scripts/swaylock/screenshot.jpg -blur 0x16 ~/Scripts/swaylock/screenshot.jpg
# now lock the screen
swaylock -f -i ~/Scripts/swaylock/screenshot.jpg
