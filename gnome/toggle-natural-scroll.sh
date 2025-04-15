#!/bin/bash

CURRENT_SETTING="$(gsettings get org.gnome.desktop.peripherals.mouse natural-scroll)"

if [ "$CURRENT_SETTING" == "true" ]; then
    # disable natural scroll
    gsettings set org.gnome.desktop.peripherals.mouse natural-scroll false
    gsettings set org.gnome.desktop.peripherals.mouse speed "-0.516447368421"
    notify-send "gsettings" "Disabled trackball mode!"
else
    # enable natural scroll
    gsettings set org.gnome.desktop.peripherals.mouse natural-scroll true
    gsettings set org.gnome.desktop.peripherals.mouse speed "-0.25"
    notify-send "gsettings" "Enabled trackball mode!"
fi
