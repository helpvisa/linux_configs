#!/bin/sh

POWERSTATUS=$(powerprofilesctl get)
notify-send "power profile set to $POWERSTATUS"
