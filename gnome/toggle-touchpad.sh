#!/bin/sh

STATUS=$(gsettings get org.gnome.desktop.peripherals.touchpad send-events)

if [ "$STATUS" = "'enabled'" ]; then
    gsettings set org.gnome.desktop.peripherals.touchpad send-events "disabled"
    notify-send "gsettings" "Touchpad Disabled" -t 2500
else
    gsettings set org.gnome.desktop.peripherals.touchpad send-events "enabled"
    notify-send "gsettings" "Touchpad Enabled" -t 2500
fi
