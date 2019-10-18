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

# Cute pipe to xclip to pipe to clipboard!
alias copy='xclip -sel clip'

# Interesting network stuff
alias i2p='i2prouter console'
alias freenet='java -jar new_installer_offline.jar'

# Hack to remove wifi driver from kernal space and put back in I think - hard restart
alias fixwifi='sudo modprobe -r mwifiex_pcie && sudo modprobe mwifiex_pcie'

# Work
alias fl='cds ~/go/src/github.com/getlantern/flashlight'
alias ui='cds ~/go/src/github.com/getlantern/lantern-desktop-ui'
alias lb='cds ~/go/src/github.com/getlantern/lantern-build'
alias lt='cds ~/go/src/github.com/getlantern/load-testing'
