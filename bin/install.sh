#!/bin/sh

# Goal is to be idempotent, where I can run this script as many times as I want
# and nothing bad will happen

# Install X-Code CLI Utilities
xcode-select --install

# Install brew with noninteractive
# To-Do: check if brew is already installed and skip if so
NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Clone repository
git clone https://github.com/landerwells/dotfiles.git

# source zsh files
source ~/dotfiles/.zprofile

# brew should now work so install stow
brew install stow
cd ~/dotfiles
stow .
