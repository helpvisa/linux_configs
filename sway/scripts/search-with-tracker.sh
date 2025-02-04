#!/usr/bin/bash

RESULTS=$(tracker3 search --disable-snippets --disable-color "$(fuzzel --dmenu)")
TRIMMED=$(echo "$RESULTS" | tail -n+2)

if [[ -z "$TRIMMED" ]]; then
	echo 'No indexed files found!' | fuzzel --dmenu
else
	URL="$(echo "$TRIMMED" | fuzzel --dmenu)"
	URL="$(echo "$URL" | xargs)"
	xdg-open "$URL"
fi
