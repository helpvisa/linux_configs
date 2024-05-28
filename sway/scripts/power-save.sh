#!/bin/sh

powerprofilesctl set power-saver

POWERSTATUS=$(powerprofilesctl get)
notify-send "power profile set to $POWERSTATUS"
