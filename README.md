# TODO
- dmenu scripts
- keymap util (dmenu or md file)
- new st default font and C+up/down support
- .Xmodmap switch caps and ctrl
- i3 and vim configs?
- encrypted secrets?
- more cron jobs?
- urxvt?

# Usage

## Install

To install you must have a clear $HOME dir
```
git clone --bare https://github.com/bcmertz/dotfiles.git $HOME/.cfg
```

## Shortcuts

Add some alias like

```
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

See `~/.bash_aliases` cfg and git aliases, as well as `~/.local/bin/git_alias`



# References

For more information on the git bare repo structure used here, and for references to what inspired the structure of this repo:

- https://news.ycombinator.com/item?id=11071754
- https://www.atlassian.com/git/tutorials/dotfiles
