#!/bin/bash

function die() {
  echo "$@"
  exit 1
}

# bashrc
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.bash_aliases ~/.bash_aliases
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.coderc ~/.coderc

# applications
for f in $(ls -d ~/dotfiles/.local/share/applications/*); do ln -s $f ~/.local/share/applications; done

# config
ln -s ~/dotfiles/.config/mimeapps.list ~/.config/mimeapps.list
ln -s ~/dotfiles/.config/redshift.conf ~/.config/redshift.conf

# git
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.gitignore_global ~/.gitignore_global

# install custom scripts
mkdir -p ~/.time # for time on computer cronjob
for f in $(ls -d ~/dotfiles/.local/bin/*); do ln -s $f ~/.local/bin; done
for f in $(ls -d ~/dotfiles/.local/bin/cron/*); do ln -s $f ~/.local/bin/cron; done
# x
ln -s ~/dotfiles/.xinitrc ~/.xinitrc

# lantern
ln -s ~/dotfiles/.lanternrc ~/.lanternrc || echo ".lanternrc doesn't exist"
