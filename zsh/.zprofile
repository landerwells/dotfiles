# Set PATH, MANPATH, etc., for Homebrew.
#
# Check if Linux or Mac
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

source $HOME/.config/zsh/.zshrc
source ~/.config/dotfiles/exports

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
