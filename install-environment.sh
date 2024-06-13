#!/bin/bash

# install brew 
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# install zsh
brew install zsh

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

# install starship
if type "starship" > /dev/null 2>&1; then
    echo "starship is already installed !"
else 
    curl -sS https://starship.rs/install.sh | sh
fi

# install editor
brew install neovim vim

# install orbstack (Docker)
brew install --cask orbstack

# install maqqy (clipboard manager)
brew install --cask maccy

# PHP environment
# install symfony
curl -sS https://get.symfony.com/cli/installer | bash

# install php 8.2
brew install php@8.2 rabbitmq-c

# some tool
brew install ripgrep bat exa fd gh pandoc zoxide

# marp cli
brew install marp-cli
