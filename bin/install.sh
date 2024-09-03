#!/bin/sh

# Goal is to be idempotent, where I can run this script as many times as I want
# and nothing bad will happen

# Install X-Code CLI Utilities
xcode-select --install

# Clone repository
git clone https://github.com/landerwells/dotfiles.git

# Create bin directory
mkdir ~/bin

NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"


