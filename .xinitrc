#!/bin/sh

# do this to make plank / xfce happy
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# do this to make plank / xfce happy
XDG_SESSION_TYPE=x11
export XDG_SESSION_TYPE


# merge in defaults and keymaps
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# configure trackpad and other X input
[ -f ~/.xinput ] && . ~/.xinput

# default to i3
default='/usr/share/xsessions/i3.desktop'
# read in session
session=${1:-$default}
# get the name of the wm session and set it
export SESSION=$(echo $session | xargs basename | cut -d '.' -f 1)

# get line formatted like Exec=i3 for startup command
startup_command=$(grep -w 'Exec' $session | cut -d '=' -f 2)

# enable these programs at the start of X
[ -f ~/.xprofile ] && . ~/.xprofile

exec $startup_command
