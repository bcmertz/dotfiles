#!/bin/bash

LOCK_FILE="$HOME/.cache/eww-info-center.lock"
EWW_BIN="$HOME/.local/bin/eww"

run() {
	eww open info-center
	sleep 0.2
	eww update icenter=true
}

# Run eww daemon if not running
if [[ ! `pidof eww` ]]; then
	eww daemon
	sleep 1
else
	if [[ ! -f "$LOCK_FILE" ]]; then
		touch "$LOCK_FILE"
		run
	else
		eww update icenter=false
		sleep 0.6
		eww close info-center
		rm "$LOCK_FILE"
	fi
fi
