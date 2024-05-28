#!/bin/sh

powerprofilesctl set performance

POWERSTATUS=$(powerprofilesctl get)
notify-send "power profile set to $POWERSTATUS"
