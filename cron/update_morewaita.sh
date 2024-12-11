#!/usr/bin/dash

cd /home/helpvisa/Compiles/MoreWaita || exit
if [ "$(git remote update && git diff origin/main)" ]; then
	git pull
	notify-send "MoreWaita is ready to be updated." "Please run install.sh with sudo!" -a "cronie" -u normal
else
	notify-send "MoreWaita is up to date." "Enjoy your life!" -a "cronie" -u normal
fi
