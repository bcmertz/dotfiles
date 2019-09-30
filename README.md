## System setup:

- OS : manjaro
- WM : i3 gaps
- Compositor: compton
- Editor : emacs
- Terminal : st
- Status : polybar
- Finder : ranger
- Misc : redshift, dmenu, dunst, firefox/surf, ...

## Usage

To install you must have a clear $HOME dir
```
git clone --bare https://github.com/bcmertz/dotfiles.git $HOME/.cfg
```

Add some alias like

```
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

See `~/.bash_aliases` cfg and git aliases, as well as `~/.local/bin/git_alias`

## TODO
- Fix up fonts
- st C+up/down support
- .Xmodmap switch caps and ctrl
- keymap reference (dmenu or md file)
- encrypted secrets?

## References

For more information on the git bare repo structure used here, and for references to what inspired the structure of this repo:

- https://news.ycombinator.com/item?id=11071754
- https://www.atlassian.com/git/tutorials/dotfiles

