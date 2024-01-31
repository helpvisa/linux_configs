#!/bin/sh

# execute startup programs
xset s off &
xset dpms 0 0 0 &
xss-lock --transfer-sleep-lock -- i3lock --color 112233 --nofork &
nm-applet &
xrandr --dpi 120 &
xinput set-prop 'pointer:Razer Razer DeathAdder V2' 'libinput Accel Speed' -1.0 &
nitrogen --restore &
redshift &
picom &
dunst &
~/Compiles/dwm-bar/dwm_bar.sh

# execute dwm
/usr/local/bin/dwm
