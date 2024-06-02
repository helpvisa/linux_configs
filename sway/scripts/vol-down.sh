#!/bin/sh

wpctl set-volume @DEFAULT_SINK@ 10%-

VOLUME=$(wpctl get-volume @DEFAULT_SINK@)
if [[ "$VOLUME" == *"MUTED"* ]]; then
	VOLUME=$(qalc -t $(echo $VOLUME | awk 'OFS=" " {print $2}') times 100)
	notify-send "AUDIO MUTED!" -h int:value:$VOLUME -h string:synchronous:volume -e
else
        VOLUME=$(qalc -t $(echo $VOLUME | awk 'OFS=" " {print $2}') times 100)
        notify-send "VOL: " -h int:value:$VOLUME -h string:synchronous:volume -e
fi
