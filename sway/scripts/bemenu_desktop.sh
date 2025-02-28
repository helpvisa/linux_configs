#!/bin/sh

LAUNCHER=" \
    BEMENU_BACKEND=wayland bemenu -i -l 30 -f \
    -H 24 \
    --counter=always \
    -p 'launch <>' \
    --fn 'Input Mono 12' \
    --tb='#222222' \
    --fb='#222222' \
    --cb='#222222' \
    --nb='#222222' \
    --hb='#333333' \
    --fbb='#222222' \
    --sb='#222222' \
    --ab='#222222' \
    --scb='#222222' \
    --tf='#222222' \
    --tb='#d97f2b' \
    --hf='#d97f2b'"

j4-dmenu-desktop --dmenu="$LAUNCHER" --term foot --display-binary
