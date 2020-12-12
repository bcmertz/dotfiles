# Dotfiles
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Git
alias gs='git_alias status'
alias ga='git_alias add'
alias gc='git_alias commit'
alias gd='git_alias diff'
alias gb='git_alias branch'
alias gp='git_alias push'
alias gpl='git_alias pull'
alias gch='git_alias checkout'

# devour opened files
alias zathura='devour zathura'
alias feh='devour feh'
alias mpv='devour mpv'
alias vlc='devour vlc'

alias za='zathura'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -a --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='/usr/bin/ls --color=auto'

# less annoying tree
alias tree="tree -a -I 'node_modules|.git'"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Utility aliases
alias c='clear'
alias s='source ~/.bashrc'
alias ag='ag --hidden --ignore node_modules --ignore .git'
alias copy='xclip -sel clip'
alias r='fc -s'
alias rc='fc -s | copy'
alias wcdir='wc -w *'
alias reboot='shutdown -r now'

# Hack to remove wifi driver from kernal space and put back in I think - hard restart
alias fixwifi='sudo modprobe -r mwifiex_pcie && sudo modprobe mwifiex_pcie'

# arduino
alias fixarduino='sudo chmod a+rw /dev/ttyACM0'

ex () {
     if [ -f "$1" ] ; then
         case "$1" in
             *.tar.bz2)   tar xjf "$1"     ;;
             *.tar.gz)    tar xzf "$1"     ;;
             *.bz2)       bunzip2 "$1"     ;;
             *.rar)       rar x "$1"       ;;
             *.gz)        gunzip "$1"      ;;
             *.tar)       tar xf "$1"      ;;
             *.tbz2)      tar xjf "$1"     ;;
             *.tgz)       tar xzf "$1"     ;;
             *.zip)       unzip "$1"       ;;
             *.Z)         uncompress "$1"  ;;
             *.7z)        7z x "$1"    ;;
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}