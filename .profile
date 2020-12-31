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
export EDITOR="emacsclient -t"
export VISUAL="emacs"

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# theme files
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc-2.0"
export GTK3_RC_FILES="$HOME/.config/gtk-3.0/gtkrc-3.0"

# get bash defaults
source ~/.bashrc

# choose window manager, get basename, and remove .desktop
wm=$(wmsel)

# weird annoying thing that's necessary
alias startx='startx ~/.xinitrc'

# start Xorg
if [[ "$wm" != "" ]]; then
    # if a session argument is chosen, startx appropriately
    startx $wm
fi
