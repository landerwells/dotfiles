# Source bashrc
source ~/.config/bash/bashrc

export XDG_CONFIG_HOME="$HOME/.config"
export INPUTRC=~/.config/bash/inputrc
export LESSHISTFILE=""
export CARGO_HOME=$HOME/Toolbox/cargo
export RUSTUP_HOME=$HOME/Toolbox/rustup
# need to configure for bash and mac differences
. "$HOME/Toolbox/cargo/env"

[ -f "/home/landerwells/.ghcup/env" ] && . "/home/landerwells/.ghcup/env" # ghcup-env
. "/home/lander-wells/Toolbox/cargo/env"
