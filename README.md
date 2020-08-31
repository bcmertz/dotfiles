## Usage

### Cloning

To install you must have a clear $HOME dir
```
git clone --bare https://github.com/bcmertz/dotfiles.git $HOME/.cfg
```

Add some alias like

```
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

See `~/.bash_aliases` cfg and git aliases, as well as `~/.local/bin/git_alias`


Install dependencies
```
~/.local/bin/installpackagelist
```

Setup GPG keys and trust db from backup
```
~/.local/bin/setup/setup
```

### Backing up

Document Dependencies
```
~/.local/bin/makepackagelist
```

Backup GPG keys and trust db
```
~/.local/bin/setup/backup
echo "now copy to some device like a thumbdrive"
```

Backup photos, documents, etc
```
~/.local/bin/cron/backupfiles
```

## References

For more information on the git bare repo structure used here, and for references to what inspired the structure of this repo:

- https://news.ycombinator.com/item?id=11071754
- https://www.atlassian.com/git/tutorials/dotfiles

