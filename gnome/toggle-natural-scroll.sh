#!/bin/bash

CURRENT_SETTING="$(gsettings get org.gnome.desktop.peripherals.mouse natural-scroll)"
ID=$(cat /tmp/toggle-trackball-id 2>/dev/null)

if [ "$CURRENT_SETTING" == "true" ]; then
    # disable natural scroll
    gsettings set org.gnome.desktop.peripherals.mouse natural-scroll false
    # gsettings set org.gnome.desktop.peripherals.mouse speed "-0.516447368421"
    COMMAND="notify-send -a 'gsettings' 'OFF' 'Natural scroll disabled!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-trackball-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-trackball-id"
    fi
    eval "$COMMAND"
else
    # enable natural scroll
    gsettings set org.gnome.desktop.peripherals.mouse natural-scroll true
    # gsettings set org.gnome.desktop.peripherals.mouse speed "-0.25"
    COMMAND="notify-send -a 'gsettings' 'ON' 'Natural scroll enabled!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-trackball-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-trackball-id"
    fi
    eval "$COMMAND"
fi
