#!/usr/bin/env bash

DIRS=(
  "$HOME/Developer"
  "$HOME"
)

if [[ $# -eq 1 ]]; then
  selected=$1
else
  selected=$(find "${DIRS[@]}" -maxdepth 1 -type d | sk --margin 10% --color="bw")
fi

[[ -z $selected ]] && exit 0

selected_name=$(basename "$selected" | tr . _)

if ! tmux has-session -t "$selected_name" 2>/dev/null; then
  tmux new-session -ds "$selected_name" -c "$selected"
fi

tmux switch-client -t "$selected_name"
