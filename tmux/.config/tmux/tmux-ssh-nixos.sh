#!/usr/bin/env bash

SESSION="nixos"
HOST="landerwells@nixos"   # change to your ssh host (e.g. user@hostname)

# If the session doesn't exist, create it and run ssh inside
if ! tmux has-session -t "$SESSION" 2>/dev/null; then
  tmux new-session -ds "$SESSION" "ssh $HOST"
fi

# Attach/switch to the session
tmux switch-client -t "$SESSION" 2>/dev/null || tmux attach -t "$SESSION"
