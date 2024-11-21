#!/usr/bin/env bash

gh repo clone $(gh repo list -L 100 | fzf | cut -d' ' -f1)

