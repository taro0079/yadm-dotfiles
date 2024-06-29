#!/bin/zsh

tmpfile="/tmp/yazi-choice.txt"

rm -f $tmpfile

yazi --chooser-file=$tmpfile

zellij action toggle-floating-panes
zellij action write 27
zellij action write-chars ":open $(cat $tmpfile)"
zellij action write 13
zellij action toggle-floating-panes
zellij action close-pane
