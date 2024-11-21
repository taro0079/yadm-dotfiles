#!/usr/bin/env bash

yadm add $(find . -type f | fzf)
