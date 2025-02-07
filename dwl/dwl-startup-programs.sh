#!/bin/bash
# launch dwl alongside clients

dex-autostart --autostart --environment sway &
dbus-update-activation-environment --all &
gnome-keyring-daemon --start &
export SSH_AUTH_SOCK
/usr/libexec/polkit-gnome-authentication-agent-1 &

swayidle -w timeout 900 '/home/helpvisa/Scripts/sway/lock-and-blur.sh' before-sleep '/home/helpvisa/Scripts/sway/lock-and-blur.sh' &
swaybg -i '/home/helpvisa/Pictures/Wallpapers/somepic.jpg' -m fill &

swaync &
wlsunset -l 43.6523 -L -79.3832 -t 3000 &
nm-applet &
blueman-applet &

# waybar &
# dwlb -ipc &
/home/helpvisa/Compiles/dwl/dwl_statusbar/dwm_bar.sh &

exec <&-
