#!/bin/sh
# script has to use name 'trackpad' since it relies on there only being one
# instance of touchpad in rc.xml

LINE=$(grep -A 1 touchpad "$HOME/.config/labwc/rc.xml" | awk 'BEGIN{RS="";FS="\n"}{print $2}')
ENABLED=$(printf "%s" "$LINE" | grep yes)

if [ -z "$ENABLED" ]; then
    sed -i '/touchpad/{n;s/no/yes/;}' "$HOME/.config/labwc/rc.xml"
    labwc --reconfigure
    notify-send "Touchpad Enabled" -t 2500
else
    sed -i '/touchpad/{n;s/yes/no/;}' "$HOME/.config/labwc/rc.xml"
    labwc --reconfigure
    notify-send "Touchpad Disabled" -t 2500
fi
