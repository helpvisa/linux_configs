#!/bin/sh
# for use in a terminal with fzf

SELECTION=$(ls -1 "$HOME/.password-store" | sed 's/.gpg//' \
    | fzf --style=minimal --layout=reverse --margin 3% --prompt='copy pass: ')

pass -c "$SELECTION"
