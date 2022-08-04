#!/bin/bash

LOCK_FILE="$HOME/.cache/eww-notification-center.lock"

run() {
	eww open notification-center
	sleep 0.2
	eww update noticenter=true
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
		eww update noticenter=false
		sleep 0.8
		eww close notification-center
		rm "$LOCK_FILE"
	fi
fi
