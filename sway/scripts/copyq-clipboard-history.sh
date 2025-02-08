#!/bin/sh

COUNT=$(( $(copyq count) - 1 ))

for i in $(seq 0 "$COUNT"); do
    WORDLIST=$(printf '%s \n ' "$WORDLIST$(copyq read "$i")")
done
echo "$WORDLIST"

SELECTION=$(printf '%s' "$WORDLIST" | fuzzel -d --index --placeholder="select from clipboard history...")
copyq select "$SELECTION"
