#!/usr/bin/env bash

mysql -u app_user -p\!ChangeMe\! -h 127.0.0.1 -D app_db -e"show full columns from  $(mysql -u app_user -p\!ChangeMe\! -h 127.0.0.1 -D app_db -e'show tables' | tail -n +2 | fzf)"

