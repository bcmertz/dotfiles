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
default='i3'
# read in chosen / default session
session=${1:-$default}
# set session
export SESSION=$session

# set gtk3 scaling once we know the session
. set_gtk_scaling

# enable these programs at the start of X
[ -f ~/.xprofile ] && . ~/.xprofile

# get line formatted like Exec=i3 for startup command
startup_command=$(grep -w 'Exec' /usr/share/xsessions/$session.desktop | cut -d '=' -f 2)

# start window manager
exec $startup_command
