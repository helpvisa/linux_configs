#!/bin/sh

# A dwm_bar function to display the current audio volume.
# Dan Brackenbury

dwm_volume() {
  # get the current volume
  VOLUME=$(wpctl get-volume @DEFAULT_SINK@ | sed -s 's/Volume: //g')
  printf "%s" "$SEP1"
  printf "vol: %s" "$VOLUME"
  printf "%s\n" "$SEP2"
}

dwm_volume
