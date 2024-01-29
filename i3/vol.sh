#!/bin/sh

VOLUME=$(wpctl get-volume @DEFAULT_SINK@)
notify-send $VOLUME
