#!/bin/bash

# install brew 
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# install zsh
brew install zsh
mkdir .zsh
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting

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
volta install node
volta install yarn@1.22.18

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
pecl install amqp # /opt/homebrew/opt/rabbitmq-c をパスに指定する

# some tool
brew install ripgrep bat exa fd gh pandoc zoxide zellij

# marp cli
brew install marp-cli

# 1password
brew install --cask 1password

# ruby environment 
brew install rbenv ruby-build

## dev environment
mkdir dev

# install font
brew install --cask font-0xproto-nerd-font

cargo install exa raycast

# git setting
git config --global user.name "Taro Morita"
git config --global user.email "morita0079@gmail.com"

# install composer
php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
php -r "if (hash_file('sha384', 'composer-setup.php') === 'dac665fdc30fdd8ec78b38b9800061b4150413ff2e3b6f88543c636f7cd84f6db9189d43a81e5503cda447da73c7e5b6') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
php composer-setup.php
php -r "unlink('composer-setup.php');"
sudo mv composer.phar /usr/local/bin/composer

# install mise
curl https://mise.run | sh
echo 'eval "$(~/.local/bin/mise activate zsh)"' >> ~/.zshrc
# install zinit (zsh plugin manager)
bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
