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
alias sxiv='devour sxiv'
alias nsxiv='devour nsxiv'
alias feh='devour feh'
alias mpv='devour mpv'
alias vlc='devour vlc'

alias za='zathura'

# Emacs config
alias emacs='emacsclient --socket-name=term -t --alternate-editor=""'  # default to terminal emacs
alias emcas='emacsclient --socket-name=term -t --alternate-editor=""'  # speeling mistake
alias emasc='emacsclient --socket-name=term -t --alternate-editor=""'  # speeling mistake
alias eamcs='emacsclient --socket-name=term -t --alternate-editor=""'  # speeling mistake

# utils
alias cp="cp -iv"
alias mv="mv -iv"
alias yt="youtube-dl --add-metadata -i"
alias yta="yt -o '~/media/music/%(title)s.%(ext)s' -x --audio-format mp3 --prefer-ffmpeg"
alias ytv-best="youtube-dl -f bestvideo+bestaudio "
alias ffmpeg="ffmpeg -hide_banner"
alias rm="rm -i"

# pacman and yay
alias pacsyu='sudo pacman -Syyu && alert "done"'    # update only standard pkgs
alias yaysua='yay -Sua --noconfirm && alert "done"' # update only AUR pkgs (yay)
alias yaysyu='yay -Syu --noconfirm && alert "done"' # update standard pkgs and AUR pkgs (yay)
alias pacunlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
alias paccleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -pa --color=auto --group-directories-first'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep -i --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alFh'
alias la='ls -A'
alias l='/usr/bin/ls -p --color=auto --group-directories-first'
alias lsd='/usr/bin/ls -a --color=auto -d */'
alias lsdir='/usr/bin/ls -a --color=auto -d */'

# less annoying tree
alias tree="tree -a -I 'node_modules|.git'"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Utility aliases
alias c='clear'
alias s='source ~/.bashrc'
alias ag='ag --hidden --ignore node_modules --ignore .git'
alias r='fc -s'
alias rc='fc -s | copy'
alias reboot='shutdown -r now'
alias ff='find_file'
alias wcdir='wc -w *'
alias wcdir_rec='wc -w **/*'
alias lcdir='wc -l *'
alias lcdir_rec='wc -l **/*'

# transmission-remote
alias t='transmission-remote'
alias tl='transmission-remote -l | less'

# signal utils
alias cont='pkill -18'
# complete -F _pgrep cont
alias stop='pkill -19'
# complete -F _pgrep stop

# Hack to remove wifi driver from kernal space and put back in I think - hard restart
alias fixwifi='sudo modprobe -r mwifiex_pcie && sudo modprobe mwifiex_pcie'

# hacky fix to make arduino serial connection permissions work
# alternative permanent solution
# sudo usermod -aG uucp <username>
alias fixarduino='sudo chmod a+rw /dev/ttyACM0'

# simple xev, hide unnecessary output
sxev () {
    xev | awk -F'[ )]+' '/^KeyPress/ { a[NR+2] } NR in a { printf "%-3s %s\n", $5, $8 }'
}

ex () {
    if [ -f "$1" ] ; then
        dest=$(echo $1 | sed 's/.zip//')
        case "$1" in
            *.tar.bz2)
                mkdir -p $dest
                tar xjf "$1" --directory=$dest     ;;
            *.tar.gz)
                mkdir -p $dest
                tar xzf "$1" --directory=$dest     ;;
            *.tbz2)
                mkdir -p $dest
                tar xjf "$1" --directory=$dest     ;;
            *.tgz)
                mkdir -p $dest
                tar xzf "$1" --directory=$dest     ;;
            *.tar.xz)
                mkdir -p $dest
                tar xf "$1" --directory=$dest      ;;
            *.tar)
                mkdir -p $dest
                tar xf "$1" --directory=$dest      ;;
            *.rar)
                mkdir -p $dest
                rar x "$1" $dest                   ;;
            *.zip)
                mkdir -p $dest
                unzip "$1" -d $dest                ;;
            *.7z)
                mkdir -p $dest
                7z x "$1" -o $dest                 ;;
            *.bz2)       bunzip2 "$1"                       ;;
            *.gz)        gunzip "$1"                        ;;
            *.xz)        unxz -k -v "$1"                          ;;
            *.Z)         uncompress "$1"                    ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# combo cds ls
cds() {
    if [ -z "$1" ]; then
        cd ~ && /usr/bin/ls -p --color=auto --group-directories-first
    else
        cd "$1" && /usr/bin/ls -p --color=auto --group-directories-first
    fi
}
