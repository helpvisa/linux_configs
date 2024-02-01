#!/bin/sh

pactl set-sink-mute @DEFAULT_SINK@ toggle

VOLUME=$(wpctl get-volume @DEFAULT_SINK@)
notify-send "$VOLUME"
