#!/bin/sh

BRIGHTNESS=$(brightnessctl g)
BRIGHTNESS=$(bc <<< "scale=2;$BRIGHTNESS/100" | sed 's/^\./0./')
notify-send "brightness: $BRIGHTNESS"
