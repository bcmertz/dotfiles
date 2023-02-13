# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Adds `~/.local/bin/` and all subdirectories to $PATH
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"

# set aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

# .bash_history
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
shopt -s histappend
# dont save on history on exit to prevent race condition if multiple sessions are killed simultaneously
trap 'unset HISTFILE; exit' SIGHUP
# append entered command to history after running it
PROMPT_COMMAND="history -a"
# append, then read in the history file to get other terminals input
# PROMPT_COMMAND="history -a;history -n"

# bash configuration
shopt -s checkwinsize # update rows & columns
shopt -s globstar     # ** matches all files / directories in path

# Interpret del properly
tput smkx

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Show git branch name and make pretty command line
parse_git_branch() {
    gb 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

PS1='\[\033[01;32m\]reya\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;35m\]$(parse_git_branch)\[\033[00m\]\$ ' ### ONE LINE, WITH $ AT END

wal --preview | tail -n 3
