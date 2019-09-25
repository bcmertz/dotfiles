# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# set aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=200000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Interpret del properly
tput smkx

export PATH="$HOME/.rbenv/bin:$PATH"

# cd && ls command
cds() {
    cd "$1"
    ls
}

killport() {
    kill $(lsof -t -i:"$1")
}

background() { # put it in the background and don't let it speak
    "$@" > /dev/null 2>&1 &
}

# start clipmenu (dmenu clip manager) daemon
clipmenud &

# Configure golang stuff
export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin
export GOROOT=/usr/local/go
export PATH=$PATH:$GOPATH/bin
# if not running start go docs and disown
if type godoc > /dev/null; then
    if ! pgrep -x "godoc" > /dev/null
    then
	godoc -http=:6060 </dev/null >/dev/null 2>&1 &
    fi
fi


# Configure editor info - emacs!
export PATH="$HOME/.cask/bin:$PATH" # cask package manager
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"          #open terminal emacs
export VISUAL="emacsclient -t"          #open terminal emacs
alias emacs='emacsclient -t --alternate-editor=""' #default to terminal, if want gui use line below
alias emacsgui='emacsclient -create-frame --alternate-editor=""'
# alias e='emacsclient -t --alternate-editor=""' # super duper shortcut
# alias emacs='emacs -nw' #used to allow terminal emacs to run as default

# Hack to remove wifi driver from kernal space and put back in I think - hard restart
alias fixwifi='sudo modprobe -r mwifiex_pcie && sudo modprobe mwifiex_pcie'
# Restart network manager
#alias fixwifi='sudo  systemctl restart NetworkManager.service'

 # Android path variables
#export ANDROID_HOME="/home/thistle/Android/Sdk"
#export JAVA_HOME="/opt/android-studio/jre/"
#export ANDROID_NDK_HOME="$ANDROID_HOME/ndk-bundle"
#export PATH=${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/ndk-bundle:$ANDROID_HOME/platform-tools:/opt/android-studio/jre/bin/:$JAVA_HOME/bin
export PYTHONPATH=$HOME/go/src/github.com/getlantern/lantern_aws/lib:$HOME/go/src/github.com/getlantern/secret_lib:$HOME/go/src/github.com/getlantern/config-server/etc

# if ~/.lanternrc exists add in its info
if [ -f $HOME/.lanternrc ]; then
    . $HOME/.lanternrc
fi

# Show git branch name and make pretty command line
parse_git_branch() {
 git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1='\[\033[01;32m\]leila\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;35m\]$(parse_git_branch)\[\033[00m\]\$ ' ### ONE LINE, WITH $ AT END    
#PS1='\[\033[01;32m\]leila\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;35m\]$(parse_git_branch)\[\033[00m\]\n$\[$(tput sgr0)\] '
