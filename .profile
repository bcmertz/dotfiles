# Adds `~/.local/bin/` and all subdirectories to $PATH
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export EDITOR="emacs"
export TERMINAL="st"
export BROWSER="surf"
export READER="zathura"
export FILE="nemo"

export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

