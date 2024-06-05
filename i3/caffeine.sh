#!/bin/sh

STATUS=$(cat /run/user/$(id -u)/caffeine.txt)

if [[ "$STATUS" == *"disabled"* ]]; then
    xset s off
    xset -dpms
    echo "enabled" > /run/user/`id -u`/caffeine.txt
    echo "sipping some coffee..."
    notify-send "Sipping some coffee..." -h string:synchronous:coffee -e
else
    xset +dpms
    xset s on
    echo "disabled" > /run/user/`id -u`/caffeine.txt
    echo "feeling sleepy..."
    notify-send "Feeling sleepy..." -h string:synchronous:coffee -e
fi
