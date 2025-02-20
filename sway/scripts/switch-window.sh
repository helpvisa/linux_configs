#!/bin/sh

WINDOW="$(wlrctl toplevel list | fuzzel -d)"
APP_ID="$(printf "%s" "$WINDOW" | cut -d ':' -f 1)"
NAME="$(printf "%s" "$WINDOW" | cut -d ':' -f 2)"

wlrctl toplevel focus "app_id:${APP_ID}"
# we echo to trim whitespace
echo "app_id:${APP_ID}" "title:$(echo "${NAME}")"
