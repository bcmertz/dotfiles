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
ln -s ~/dotfiles/.profile ~/.profile

# applications
for f in $(ls -d ~/dotfiles/.local/share/applications/*); do ln -s $f ~/.local/share/applications; done

# config
ln -s ~/dotfiles/.config/mimeapps.list ~/.config/mimeapps.list
ln -s ~/dotfiles/.config/redshift.conf ~/.config/redshift.conf
for f in $(ls -d ~/dotfiles/.config/git/*); do ln -s $f ~/.config/git; done
for f in $(ls -d ~/dotfiles/.config/dmenu/*); do ln -s $f ~/.config/dmenu; done

# git
ln -s ~/dotfiles/.gitconfig ~/.gitconfig

# install custom scripts
for f in $(ls -d ~/dotfiles/.local/bin/*); do ln -s $f ~/.local/bin; done
for f in $(ls -d ~/dotfiles/.local/bin/cron/*); do ln -s $f ~/.local/bin/cron; done
# x
ln -s ~/dotfiles/.xinitrc ~/.xinitrc

# lantern
ln -s ~/dotfiles/.lanternrc ~/.lanternrc || echo ".lanternrc doesn't exist"


# build_configs [TODO]
