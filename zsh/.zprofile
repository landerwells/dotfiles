# Set PATH, MANPATH, etc., for Homebrew.
#
# Check if Linux or Mac

export XDG_CONFIG_HOME="$HOME/.config"
export INPUTRC=~/.config/bash/inputrc
export ZDOTDIR="$XDG_CONFIG_HOME/dotfiles/zsh"
export LESSHISTFILE=""
export VISUAL=nvim
export EDITOR="$VISUAL"
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

source ~/.config/dotfiles/exports
# source $HOME/.config/zsh/.zshrc

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env

echo "hello from zprofile"
