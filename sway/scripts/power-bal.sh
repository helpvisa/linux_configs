#!/bin/sh

powerprofilesctl set balanced

POWERSTATUS=$(powerprofilesctl get)
notify-send "power profile set to $POWERSTATUS"
