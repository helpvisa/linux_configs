#!/bin/sh

wpctl set-mute @DEFAULT_SOURCE@ toggle

VOLUME=$(wpctl get-volume @DEFAULT_SOURCE@)
notify-send "$VOLUME"
