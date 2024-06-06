#!/bin/bash

# install tpm (Tmux Plugin Manager)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# install fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# Install Cargo
if type "cargo" > /dev/null 2>&1; then
	echo "rustup is already installed !"
else 
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	source $HOME/.cargo/env
fi

# Install Volta 
if type "volta" > /dev/null 2>&1; then
	echo "volta is already installed !"
else 
	curl https://get.volta.sh | bash
fi
