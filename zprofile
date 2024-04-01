# Set PATH, MANPATH, etc., for Homebrew.
#
# Check if Linux or Mac
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Source zshrc
source $HOME/.config/zsh/.zshrc

export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export ZSH_COMPDUMP="$HOME/.cache/.zcompdump"
export CARGO_HOME=$HOME/Toolbox/cargo
export RUSTUP_HOME=$HOME/Toolbox/rustup
export LESSHISTFILE=-

export PATH=$PATH:$CARGO_HOME/bin
source "$CARGO_HOME/env"

