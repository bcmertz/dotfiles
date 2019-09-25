#!/bin/bash

# # bashrc
ln -s ~/dotfiles/.bashrc ~/.bashrc

# # applications
for f in $(ls -d ~/dotfiles/.local/share/applications/*); do ln -s $f ~/.local/share/applications; done

# profile
ln -s ~/dotfiles/.profile ~/.profile

# config
ln -s ~/dotfiles/.config/mimeapps.list ~/.config/mimeapps.list
ln -s ~/dotfiles/.config/redshift.conf ~/.config/redshift.conf

# git
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.gitignore_global ~/.gitignore_global

# install custom scripts
mkdir -p ~/.time # for time on computer cronjob
for f in $(ls -d ~/dotfiles/.local/bin/*); do ln -s $f ~/.local/bin; done

# x
ln -s ~/dotfiles/.xinitrc ~/.xinitrc
