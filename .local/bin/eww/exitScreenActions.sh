#!/bin/bash

DATE=$(date '+%b%d-%H-%M:%S.png');

hide_unhide_windows() {
	while bspc node any.hidden.window -g hidden=off; do false; done && while bspc node 'any.!hidden.window' -g hidden=on; do :; done
}

pre_run() {
	if [[ -f "$HOME/.cache/eww-escreen.lock" ]]; then
		eww update escreen=false
		sleep 0.8
		$HOME/.local/bin/tglbar
		hide_unhide_windows
		eww close exit-screen
		rm "$HOME/.cache/eww-escreen.lock"
	fi
}

run() {
	pre_run && sleep 0.2
	systemctl $1
}

case $1 in
	"poweroff")
		run "poweroff" &
		;;
	"reboot")
		run "reboot" &
		;;
	"suspend")
		run "suspend" &
		;;
	"hibernate")
		run "hibernate" &
		;;
	"logout")
		pre_run && sleep 0.2
		bspc quit
		;;
esac
