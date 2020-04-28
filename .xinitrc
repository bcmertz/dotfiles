#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

# merge in defaults and keymaps

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# configure trackpad and other X input
[ -f ~/.xinput ] && . ~/.xinput

# enable these programs at the start of X
[ -f ~/.xprofile ] && . ~/.xprofile

exec i3
#exec emacs
