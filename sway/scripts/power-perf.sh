#!/bin/sh

# powerprofilesctl set performance

# POWERSTATUS=$(powerprofilesctl get)
# notify-send "power profile set to $POWERSTATUS"

tuned-adm profile throughput-performance

POWERSTATUS=$(tuned-adm active)
notify-send "$POWERSTATUS"
