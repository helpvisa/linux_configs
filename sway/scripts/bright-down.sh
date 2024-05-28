#!/bin/sh

brightnessctl set 5%-

BRIGHTNESS=$(brightnessctl g)
notify-send "brightness: $BRIGHTNESS"
