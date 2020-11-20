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

export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

source ~/.bashrc

# (1) prompt user, and read command line argument for window manager
read -p $'Which window manager? [default] i3 [1] emacs [2] bspwm [3] cinnamon [4] kde [5] xfce\x0a' wm

# (2) handle the command line argumen given for window manager
while true
do
  case $wm in
   [1]* ) wm="emacs"
           echo "Starting emacs"
           break;;

   [2]* ) wm="bspwm"
          echo "starting bspwm"
          break;;

   [3]* ) wm="cinnamon"
          echo "starting cinnamon"
          break;;

   [5]* ) wm="xfce"
          echo "starting xfce"
          break;;

   * ) wm="i3"
       echo "starting i3"
       break ;;
  esac
done

alias startx='startx ~/.xinitrc'

if [[ "$wm" != "" ]]; then
    # if a session argument is chosen, startx appropriately
    startx $wm
fi
