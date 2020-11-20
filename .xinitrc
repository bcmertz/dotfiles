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

# choose wm, default to i3
session=${1:-i3}
export SESSION=$session

# enable these programs at the start of X
[ -f ~/.xprofile ] && . ~/.xprofile

case $session in
   emacs             ) exec emacs;;
   i3                ) exec i3;;
   bspwm             ) exec bspwm;;
   xfce              ) exec startxfce4;;
   cinnamon|mint     ) exec cinnamon-session;;
esac
