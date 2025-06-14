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

export ZCOMP_DUMP=~/.cache/zcompdump
autoload -Uz compinit
compinit -d "$ZCOMP_DUMP"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

. "$HOME/.cargo/env"

# Add pyenv to your shell
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

export MANPAGER='nvim +Man!'

export VISUAL=nvim
export EDITOR=nvim
export XDG_CONFIG_HOME=~/.config

export PATH="$PATH:/Users/landerwells/fvm/versions/stable/bin"

source "$XDG_CONFIG_HOME/shell/aliases"
source "$XDG_CONFIG_HOME/shell/exports"

if [ -n "$TMUX" ]; then
  HISTFILE=~/.cache/zsh/history
fi

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.cache/zsh_history  # History in cashe directory
setopt auto_cd                 # If a command isn't valid, but is a directory, cd to that dir
setopt auto_pushd              # Make cd push the old directory onto the directory stack
setopt pushd_ignore_dups       # Don’t push multiple copies of the same directory onto the directory stack
setopt pushd_silent            # Don’t print the directory stack after pushd or popd
setopt append_history          # Append to history file
setopt extended_history        # Write the history file in the ':start:elapsed;command' format
setopt hist_expire_dups_first  # Expire a duplicate event first when trimming history
setopt hist_find_no_dups       # Don't display a previously found event
setopt hist_ignore_all_dups    # Delete an old recorded event if a new event is a duplicate
setopt hist_ignore_dups        # Don't record an event that was just recorded again
setopt hist_ignore_space       # Don't record an event starting with a space
setopt hist_no_store           # Don't store history commands
setopt hist_reduce_blanks      # Remove superfluous blanks from each command line being added to the history list
setopt hist_save_no_dups       # Don't write a duplicate event to the history file
setopt hist_verify             # Don't execute immediately upon history expansion
setopt inc_append_history      # Add commands to the history file as they are executed
setopt auto_resume             # Attempt to resume existing job before creating a new process
setopt share_history
setopt autoparamslash
unsetopt hist_beep             # Don't beep when attempting to access a missing history entry

# alias d='dirs -v'
# for index ({1..9}) alias "$index"="cd +${index}"; unset index

#-------------------------------------------------------------------------------
#               VI-MODE
#-------------------------------------------------------------------------------
bindkey -v
export KEYTIMEOUT=1

bindkey '^?' backward-delete-char
# Need to figure out why when searching can't backspace?

# Change cursor shape for different vi modes.
function zle-keymap-select {
if [[ ${KEYMAP} == vicmd ]] ||
  [[ $1 = 'block' ]]; then
  echo -ne '\e[1 q'
elif [[ ${KEYMAP} == main ]] ||
  [[ ${KEYMAP} == viins ]] ||
  [[ ${KEYMAP} = '' ]] ||
  [[ $1 = 'beam' ]]; then
  echo -ne '\e[5 q'
fi
}
zle -N zle-keymap-select
zle-line-init() {
echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Add vi-mode text objects e.g. da" ca(
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km $c select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km $c select-bracketed
  done
done

# Mimic tpope's vim-surround
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -M vicmd cs change-surround
bindkey -M vicmd ds delete-surround
bindkey -M vicmd ys add-surround
bindkey -M visual S add-surround

[ -f "/Users/landerwells/.ghcup/env" ] && source "/Users/landerwells/.ghcup/env" # ghcup-env
[ -f "/Users/landerwells/.cargo/env" ] && source "/Users/landerwells/.cargo/env" # ghcup-env

# . "$HOME/.cargo/env"
#
function swap()         
{
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE
  mv "$2" "$1"
  mv $TMPFILE "$2"
}

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

# Load Starship
eval "$(starship init zsh)"

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

if [ -d "/opt/homebrew" ]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
  source /opt/homebrew/share/zsh-autopair/autopair.zsh
fi
