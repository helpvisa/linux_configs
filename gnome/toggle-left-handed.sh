#!/bin/bash

CURRENT_SETTING="$(gsettings get org.gnome.desktop.peripherals.mouse left-handed)"
ID=$(cat /tmp/toggle-trackball-id 2>/dev/null)

if [ "$CURRENT_SETTING" == "true" ]; then
    gsettings set org.gnome.desktop.peripherals.mouse left-handed false
    COMMAND="notify-send -a 'gsettings' 'OFF' 'Disabled leftie mode!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-trackball-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-trackball-id"
    fi
    eval "$COMMAND"
else
    gsettings set org.gnome.desktop.peripherals.mouse left-handed true
    COMMAND="notify-send -a 'gsettings' 'ON' 'Enabled leftie mode!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-trackball-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-trackball-id"
    fi
    eval "$COMMAND"
fi
