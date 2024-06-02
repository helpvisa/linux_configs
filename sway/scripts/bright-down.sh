#!/bin/sh

brightnessctl set 5%-

BRIGHTNESS=$(brightnessctl -P get)
notify-send "BKLT " -h int:value:$BRIGHTNESS -h string:synchronous:backlight -e
