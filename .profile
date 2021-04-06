#!/bin/bash
#
# on login

# Adds `~/.local/bin/` and all subdirectories to $PATH and $Home/bin for appimages
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//'):$HOME/bin"
export TERMINAL="st-meta-256color"
export BROWSER="firefox"
export READER="zathura"
export FILE="pcmanfm"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t --socket-name=term"
export VISUAL="emacsclient --socket-name=gui"

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# theme files
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK3_RC_FILES="$HOME/.config/gtk-3.0/gtkrc-3.0"

# get bash defaults
source ~/.bashrc

# choose window manager
wm=$(wmsel)

# check if something was chosen
if [[ "$wm" != "" ]]; then
    # see if a wayland session was chosen
    wayland_session=$(echo $wm | grep '\-w' | sed 's/ -w//g')
    # if a wayland -w flag is passed in, execute it
    if [[ ! -z "$wayland_session" ]]; then
        export SESSION=$wayland_session
        startup_command=$(grep -w 'Exec' /usr/share/wayland-sessions/$wayland_session.desktop | cut -d '=' -f 2)
        MOZ_ENABLE_WAYLAND=1 QT_QPA_PLATFORM=wayland XDG_SESSION_TYPE=wayland exec dbus-run-session $startup_command
    else
        # weird annoying thing that's necessary
        alias startx='startx ~/.xinitrc'
        # if a x session argument is chosen, startx appropriately
        startx $wm
    fi
fi
