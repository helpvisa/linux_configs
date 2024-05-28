#!/bin/sh

wpctl set-volume @DEFAULT_SINK@ 10%+

VOLUME=$(wpctl get-volume @DEFAULT_SINK@)
notify-send "$VOLUME"
