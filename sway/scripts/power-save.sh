#!/bin/sh

# powerprofilesctl set power-saver

# POWERSTATUS=$(powerprofilesctl get)
# notify-send "power profile set to $POWERSTATUS"

tuned-adm profile powersave

POWERSTATUS="$(tuned-adm active)"
notify-send "$POWERSTATUS"
