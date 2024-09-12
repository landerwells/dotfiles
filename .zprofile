# Set PATH, MANPATH, etc., for Homebrew.
#
# Check if Linux or Mac

# Add bin to path
export PATH="$HOME/bin:$PATH"
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/shell/"
export LESSHISTFILE=""
export VISUAL=nvim
export EDITOR="$VISUAL"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

source ~/.config/shell/.zshrc

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
. "$HOME/.cargo/env"
