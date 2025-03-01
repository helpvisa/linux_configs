#!/bin/sh

LAUNCHER=" \
    fzf --bind=enter:replace-query+print-query \
    --style=minimal --layout=reverse \
    --margin 3% \
    --prompt='launch: '"

j4-dmenu-desktop --dmenu="$LAUNCHER" --term-mode custom --term "/bin/sh -c {cmdline@}" --display-binary
