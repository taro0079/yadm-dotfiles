#!/usr/bin/env bash

git stash apply $(git stash list | fzf | cut -d: -f1)
