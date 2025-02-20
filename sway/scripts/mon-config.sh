#!/bin/bash

# adjust screen settings
sleep 1
wlr-randr --output eDP-1 --mode 1920x1080@60 --adaptive-sync enabled
# reset desktop background
# swaybg -i /home/helpvisa/Pictures/wallpapers/pic.jpg -m fill &
swaybg -c 704216 &
wlsunset -l 43.6523 -L -79.3832 -t 3000 &
