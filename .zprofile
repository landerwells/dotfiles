export EDITOR=nvim
export LESSHISTFILE=""
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.pyenv/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PYTHONHISTORY="$HOME/.cache/python_history"
export VISUAL=nvim
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"

# if [[ "$OSTYPE" == "linux-gnu"* ]]; then
#   eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
if [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  . "$HOME/.cargo/env"
fi


# Add pyenv to your shell
# eval "$(pyenv init --path)"
# eval "$(pyenv init -)"
