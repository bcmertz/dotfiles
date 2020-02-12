To use: `crontab -e` 

* * * * * ~/.local/bin/cron/datelog
0,15,30,45 * * * * ~/.local/bin/cron/setbg

or if you need more context for your commands

* * * * * export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus; export DISPLAY=:0; . $HOME/.bash_profile; ~/.local/bin/cron/datelog
