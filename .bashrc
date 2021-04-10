# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# set aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

# set coding environment
[ -f ~/.coderc ] && . ~/.coderc

# .bash_history
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=-1
HISTFILESIZE=-1

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

PS1='\[\033[01;32m\]leila\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;35m\]$(parse_git_branch)\[\033[00m\]\$ ' ### ONE LINE, WITH $ AT END
