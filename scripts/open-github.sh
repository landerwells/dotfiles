#!/usr/bin/env bash

# Get the current pane's path
dir=$(tmux display-message -p "#{pane_start_path}")

# Make sure the directory exists
if [[ -z "$dir" || ! -d "$dir" ]]; then
  echo "Could not determine the tmux pane's path"
  exit 1
fi

cd "$dir" || exit

# Get the Git remote URL
url=$(git remote get-url origin 2>/dev/null)

if [[ -z "$url" ]]; then
  echo "No Git remote found"
  exit 1
fi

# Check if it's a GitHub repository
if [[ $url == *"github.com"* ]]; then
  # Convert SSH URL to HTTPS if needed
  url=${url/:///\/}      # e.g., git@github.com:user/repo.git -> git/github.com/user/repo.git
  url=${url/git@/https://}
  url=${url/.git/}
  open "$url"
else
  echo "This repository is not hosted on GitHub"
fi
