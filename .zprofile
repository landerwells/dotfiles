# Set PATH, MANPATH, etc., for Homebrew.
#
# Check if Linux or Mac

# Add bin to path
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.pyenv/bin:$PATH"
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/shell/"
export LESSHISTFILE=""
export VISUAL=nvim
export EDITOR="$VISUAL"
export PYTHONHISTORY="$HOME/.cache/python_history"


if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

. "$HOME/.cargo/env"

# Add pyenv to your shell
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

