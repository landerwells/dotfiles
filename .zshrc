
if [ -n "$TMUX" ]; then
  HISTFILE=~/.cache/zsh_history
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

PROMPT='%F{red}[%F{yellow}%n%F{green}@%F{blue}%m %F{magenta} %~%F{red}]%f$ '

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

# Git Commands
alias gs='git status'
alias ga='git add'
alias gp='git push'
alias gd='git diff'
alias gcm='git commit -m'
alias gl='git log --oneline'

alias ghcs='gh copilot suggest'
alias ghce='gh copilot explain'

alias ls='ls --color=auto'
alias ll='ls -l --color=auto'

# alias ls='ls -a --color=always --group-directories-first'
# alias la='ls -a --color=always --group-directories-first'
# alias ll='ls -la --color=always --group-directories-first'
# alias ltr='ls -altr --color=always --group-directories-first'
#
alias d='dirs -v'
for index ({1..9}) alias "$index"="cd +${index}"; unset index

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias cr='cargo run'
alias ct='cargo test'
alias fz='vim $(fzf)'
alias mkdir='mkdir -pv'
alias path='echo ${PATH//:/\\n}'
alias py='python3'
alias st='networkquality'
alias vi='nvim'
alias vim='nvim'
alias svim='sudo -E nvim'
# alias emacs='emacs --no-window-system'
alias xc='open -a Xcode'
alias rm='rm -i'
alias miex='iex -S mix'
# alias config='sudo -E nvim ~/dotfiles/.config/nixos/nixos/configuration.nix'
# alias rebuild='sudo nixos-rebuild switch --flake ~/dotfiles/.config/nixos/'

# Directory Commands
alias df='cd ~/dotfiles'
alias dv='cd ~/Developer'
alias nv='cd ~/dotfiles/.config/nvim'
alias tmux='tmux -f ~/.config/tmux/tmux.conf'
# alias advent='cd ~/Developer/advent_of_rust'
alias icloud='cd ~/Library/Mobile\ Documents/com~apple~CloudDocs'
alias notes='cd ~/org/roam'

alias c='clear'
alias q='exit'
# alias ds='doom sync'
# alias dsr='doom sync --rebuild'

# if [[ "$OSTYPE" == "darwin"* ]]; then
#   alias head='ghead'
#   alias paste='gpaste'
#   alias grep='ggrep'
#   alias awk='gawk'
#   alias sed='gsed'
# fi

alias g++="g++ -std=c++23 -pedantic -Wall -Weffc++ -Wextra -Wconversion -Wsign-conversion"

source ~/dotfiles/plugins/zsh-autopair/autopair.zsh
source ~/dotfiles/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/dotfiles/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
