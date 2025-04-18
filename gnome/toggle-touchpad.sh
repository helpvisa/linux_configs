#!/bin/sh

STATUS=$(gsettings get org.gnome.desktop.peripherals.touchpad send-events)
ID=$(cat /tmp/toggle-touchpad-id 2>/dev/null)

if [ "$STATUS" = "'enabled'" ]; then
    gsettings set org.gnome.desktop.peripherals.touchpad send-events "disabled"
    COMMAND="notify-send -a 'Touchpad' 'OFF' 'Touchpad disabled!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-touchpad-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-touchpad-id"
    fi
    eval "$COMMAND"
else
    gsettings set org.gnome.desktop.peripherals.touchpad send-events "enabled"
    COMMAND="notify-send -a 'Touchpad' 'ON' 'Touchpad enabled!' -t 2500 -e"
    if [ -n "$ID" ]; then
        COMMAND="$COMMAND -r $ID -p > /tmp/toggle-touchpad-id"
    else
        COMMAND="$COMMAND -p > /tmp/toggle-touchpad-id"
    fi
    eval "$COMMAND"
fi
