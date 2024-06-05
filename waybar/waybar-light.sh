#!/bin/sh

cp /home/helpvisa/.config/waybar/light.css /home/helpvisa/.config/waybar/style.css
pkill --signal SIGUSR2 waybar
