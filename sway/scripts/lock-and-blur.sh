#!/bin/sh

# create arrays corresponding to each output's resolutions / positions
NAMES=($(swaymsg -pt get_outputs | grep 'Output' | cut -f2 -d ' '))
RESOLUTIONS=($(swaymsg -pt get_outputs | grep 'Current mode:' | cut -f5 -d ' '))
POSITIONS=($(swaymsg -pt get_outputs | grep 'Position:' | cut -f4 -d ' '))
TRANSFORMS=($(swaymsg -pt get_outputs | grep 'Transform:' | cut -f4 -d ' '))
# store a command verb to use with swaylock
COMMAND=""

for i in ${!NAMES[@]}
do
	# find the values corresponding to the current monitor
	LOCAL_RES=${RESOLUTIONS[i]}
	LOCAL_POS=${POSITIONS[i]}
	LOCAL_TRANS=${TRANSFORMS[i]}

	# debug info
	echo Display_$i:
	echo $LOCAL_RES
	echo $LOCAL_POS
	echo $LOCAL_TRANS
	echo ____________

	# rotate by transform
	if [[ "$LOCAL_TRANS" == *"270"* ]] || [[ "$LOCAL_TRANS" == *"90"*  ]]; then
		echo "Swizzling coords!"
		LOCAL_RES="$(echo $LOCAL_RES | awk 'BEGIN {FS="x"};{print $2}')x$(echo $LOCAL_RES | awk 'BEGIN {FS="x"};{print $1}')"
	fi

	# take screenshot of the monitor
	grim -g "$LOCAL_POS $LOCAL_RES" - > /tmp/lock-screenshot-$i-buf.png # save as buffer in case of vips
	# blur it; uncomment line below and recomment vips line for ImageMagick version
	# convert /tmp/lock-screenshot-$i-buf.png -filter Gaussian -resize 10% -define filter:sigma=2.5 -resize 1000% /tmp/lock-screenshot-$i.png
	vips gaussblur /tmp/lock-screenshot-$i-buf.png /tmp/lock-screenshot-$i.png 25
	COMMAND="$COMMAND -i ${NAMES[i]}:/tmp/lock-screenshot-$i.png"
done

# now lock the screen
swaylock -f -c 000000 $COMMAND
