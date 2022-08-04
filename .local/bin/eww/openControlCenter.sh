#!/bin/bash

LOCK_FILE="$HOME/.cache/eww-control-center.lock"
ACTIVE_PLAYERS=$(playerctl -l | head -n 1)

fix_stacking_bug() {
	for entry in $(xdotool search --pid $(pidof eww)); do
    	xdo below -N eww-control-panel $entry
	done
}

run() {
	eww open control-center
	sleep 0.2
	xdo raise -N eww-bar
	eww update ccenter=true; fix_stacking_bug

	sleep 1 && [[ ! -z "$ACTIVE_PLAYERS" ]] && eww update mp=true
	touch "$LOCK_FILE"
}

# Run eww daemon if not running
if [[ ! `pidof eww` ]]; then
	eww daemon
	sleep 1
else
	if [[ ! -f "$LOCK_FILE" ]]; then
		run
	else
		rm "$LOCK_FILE"
		[[ ! -z "$ACTIVE_PLAYERS" ]] && eww update mp=false && sleep 0.4
		eww update ccenter=false
		sleep 0.6
		eww close control-center
		xdo lower -N eww-bar
	fi
fi
