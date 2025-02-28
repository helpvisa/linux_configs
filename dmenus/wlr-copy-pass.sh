#!/bin/sh
# for wlroots compositors

SELECTION=$(ls -1 "$HOME/.password-store" | sed 's/.gpg//' \
    | BEMENU_BACKEND=wayland bemenu -i -l 59 -f \
    -H 25 \
    --counter=always \
    -p 'copy pass <>' \
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
    --hf='#d97f2b')

pass -c "$SELECTION"
