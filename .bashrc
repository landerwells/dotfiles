source ~/.config/shell/aliases
source ~/.config/shell/exports

set -o vi

# Add `~/bin` to the `$PATH`
export PATH="$HOME/bin:$PATH";
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

# Set history size
HISTSIZE=10000
# Save all of the history
HISTFILESIZE=10000
# Set the history file location
HISTFILE="$HOME/.cache/zsh/history"
# Enable auto cd
shopt -s autocd
# Enable autocd behavior (automatic directory stack manipulation)
shopt -s cdable_vars
# Disable pushing duplicate directories onto the directory stack
shopt -u cdspell
# Disable verbose output for pushd and popd commands
shopt -s dirspell
# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;
# Append to the Bash history file, rather than overwriting it
shopt -s histappend;
# Autocorrect typos in path names when using `cd`
shopt -s cdspell;
# Ignore duplicate history entries
HISTCONTROL=ignoredups
# Ignore lines starting with spaces in the history
HISTCONTROL=ignorespace
# Ignore duplicate entries when searching history
HISTCONTROL=ignoredups
# Remove command prefix before saving it in history
shopt -s histreedit
# Save multiline commands as a single line
shopt -s cmdhist
# Save history immediately after execution
PROMPT_COMMAND='history -a'
# Execute history substitutions immediately
shopt -s histverify

test -d ~/.linuxbrew && eval "$(~/.linuxbrew/bin/brew shellenv)"
test -d /home/linuxbrew/.linuxbrew && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
# echo "eval \"\$($(brew --prefix)/bin/brew shellenv)\"" >> ~/.bashrc

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

eval "$(starship init bash)"

set show-mode-in-prompt on
set vi-cmd-mode-string "\1\e[2 q\2"
set vi-ins-mode-string "\1\e[6 q\2"
