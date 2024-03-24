#!/bin/sh

. /home/helpvisa/.config/.volume-store

VOLUME=$(mixer | grep vol | grep -o "\b:[0-9].[0-9][0-9]\b" | grep -o "\b[0-9].[0-9][0-9]\b")

if [ "$VOLUME" == "0.00" ]; then
	notify-send "unmuted! vol: $OLDVOLUME"
	mixer vol=$OLDVOLUME
else
	notify-send "muted! vol: 0.00"
	mixer vol=0
fi

echo "export OLDVOLUME=$VOLUME" > /home/helpvisa/.config/.volume-store
