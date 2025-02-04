#!/bin/bash

# powerprofilesctl set balanced

# POWERSTATUS=$(powerprofilesctl get)
# notify-send "power profile set to $POWERSTATUS"

BATTERY_STATUS="$(cat /sys/class/power_supply/BAT0/status)"

if [[ "$BATTERY_STATUS" == *"Discharging"* ]]; then
    tuned-adm profile balanced-battery
else
    tuned-adm profile balanced
fi

POWERSTATUS="$(tuned-adm active)"
notify-send "$POWERSTATUS"
