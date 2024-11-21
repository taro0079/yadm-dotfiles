#!/usr/bin/env bash

ssh $(grep 'Host\s' ~/.ssh/config | awk '{print $2}' | fzf)
