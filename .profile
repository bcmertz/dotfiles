#!/bin/bash
#
# on login

export TERMINAL="st-meta-256color"
export BROWSER="firefox"
export READER="zathura"
export FILE="pcmanfm"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t --socket-name=term"
export sudoedit="SUDO_EDITOR=\"emacsclient -nw -a ''\" sudoedit"
export VISUAL="emacsclient --socket-name=gui"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_RUNTIME_DIR=/run/user/$(id -u)
# export $(dbus-launch)

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# theme files
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK3_RC_FILES="$HOME/.config/gtk-3.0/gtkrc-3.0"

# get bash defaults
source ~/.bashrc

# set coding environment
[ -f ~/.coderc ] && . ~/.coderc

# choose window manager
wm=$(wmsel)

# firefox smooth scrolling
export MOZ_USE_XINPUT2=1

# weird annoying thing that is necessary to do here and nowhere else
# if i move it to right before startx it tries to start xterm as the wm????
alias startx='startx ~/.xinitrc'

# check if something was chosen
if [[ "$wm" != "" ]]; then
    # see if a wayland session was chosen
    wayland_session=$(echo $wm | grep '\-w' | sed 's/ -w//g')
    # if a wayland -w flag is passed in, execute it
    if [[ ! -z "$wayland_session" ]]; then
        export SESSION=$wayland_session
        # set gtk3 scaling once we know the session
        # . set_gtk_scaling
        startup_command=$(grep -w 'Exec' /usr/share/wayland-sessions/$wayland_session.desktop | cut -d '=' -f 2)
        MOZ_ENABLE_WAYLAND=1 QT_QPA_PLATFORM=wayland XDG_SESSION_TYPE=wayland exec dbus-run-session $startup_command
    else
        # if a x session argument is chosen, startx appropriately
        startx $wm
    fi
fi
