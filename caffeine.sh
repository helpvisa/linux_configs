#!/bin/sh

STATUS=$(cat /tmp/$(id -u)-caffeine.txt)

if [ "$STATUS" ] && [ -z "${STATUS##*"disabled"*}" ]; then
    xset s off
    xset -dpms
    echo "enabled" > /tmp/$(id -u)-caffeine.txt
    echo "sipping some coffee..."
    notify-send "Sipping some coffee..." -h string:synchronous:coffee -e
else
    xset +dpms
    xset s on
    echo "disabled" > /tmp/$(id -u)-caffeine.txt
    echo "feeling sleepy..."
    notify-send "Feeling sleepy..." -h string:synchronous:coffee -e
fi
