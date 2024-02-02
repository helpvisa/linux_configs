#!/bin/sh

# other autostarts
eval $(ssh-agent)
test $(which gpg-agent) && eval $(gpg-agent --daemon)

# execute startup programs
xset s off &
xset dpms 0 0 0 &
xss-lock --transfer-sleep-lock -- i3lock --color 112233 --nofork &
nm-applet &
nitrogen --restore &
redshift &
picom &
dunst &

# execute dwm, end x when statusbar is killed
/usr/local/bin/dwm 2>>.dwm_log &
~/Compiles/dwm-bar/dwm_bar.sh
