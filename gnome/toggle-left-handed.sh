#!/bin/bash

CURRENT_SETTING="$(gsettings get org.gnome.desktop.peripherals.mouse left-handed)"

if [ "$CURRENT_SETTING" == "true" ]; then
    gsettings set org.gnome.desktop.peripherals.mouse left-handed false
    notify-send "gsettings" "Disabled leftie mode!"
else
    gsettings set org.gnome.desktop.peripherals.mouse left-handed true
    notify-send "gsettings" "Enabled leftie mode!"
fi
