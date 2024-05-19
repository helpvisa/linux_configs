#!/bin/sh

mixer vol=-0.05

VOLUME=$(mixer | grep vol | grep -o "\b:[0-9].[0-9][0-9]\b" | grep -o "\b[0-9].[0-9][0-9]\b")
notify-send "vol: $VOLUME"
