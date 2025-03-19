#!/bin/sh
# for use in a terminal with fzf

# SELECTION=$(ls -1 "$HOME/.password-store" | sed 's/.gpg//' \
#     | fzf --style=minimal --layout=reverse --margin 3% --prompt='copy pass: ')
SELECTION=$(find "$HOME/.password-store" -not -name '.*' -type f | \
    sed 's/.*\.password-store\///' | sed 's/\.gpg//' | \
    BEMENU_BACKEND=curses bemenu -i -l 30 \
    -H 25 \
    --counter=always \
    -p 'copy pass <>' \
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
