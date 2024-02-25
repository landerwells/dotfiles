#!/bin/bash

# Specify the source directory and destination directory
DOTFILES_REPO="$HOME/.dotfiles"
CONFIG_DIR="$HOME/.config"

# Function to create symbolic link if the destination doesn't exist
create_symlink() {
  source_dir="$1"
  destination_dir="$2"

    # Check if the destination directory already exists
    if [ -e "$destination_dir" ]; then
      echo "The destination directory $destination_dir already exists."
    else
      # Create symbolic link
      ln -s "$source_dir" "$destination_dir"
      echo "Created symbolic link for $source_dir to $destination_dir"
    fi
  }

# Promt if the user is using mac or linux
echo "Are you using Mac or Linux? (mac/linux)"
read -r os
# set os variable to mac or linux
if [ "$os" = "mac" ]; then
  os="mac"
elif [ "$os" = "linux" ]; then
  os="linux"
else
  echo "Invalid input. Please enter mac or linux."
  exit 1
fi

# Create symbolic links for specific directories
create_symlink "$DOTFILES_REPO/nvim" "$CONFIG_DIR/nvim"
create_symlink "$DOTFILES_REPO/bash" "$CONFIG_DIR/bash"
create_symlink "$DOTFILES_REPO/zsh" "$CONFIG_DIR/zsh"
create_symlink "$DOTFILES_REPO/aliases" "$CONFIG_DIR/aliases"
create_symlink "$DOTFILES_REPO/tmux" "$CONFIG_DIR/tmux"
create_symlink "$DOTFILES_REPO/git" "$CONFIG_DIR/git"
create_symlink "$DOTFILES_REPO/starship.toml" "$CONFIG_DIR/starship.toml"

# Create symbolic links for mac specific directories
if [ "$os" = "mac" ]; then
  create_symlink "$DOTFILES_REPO/yabai" "$CONFIG_DIR/yabai"
  create_symlink "$DOTFILES_REPO/skhd" "$CONFIG_DIR/skhd"
  create_symlink "$DOTFILES_REPO/karabiner" "$CONFIG_DIR/karabiner"
  create_symlink "$DOTFILES_REPO/linearmouse" "$CONFIG_DIR/linearmouse"
  create_symlink "$DOTFILES_REPO/iterm2" "$CONFIG_DIR/iterm2"
fi

create_symlink "$DOTFILES_REPO/haskeline" "$HOME/.haskeline"
create_symlink "$DOTFILES_REPO/profile" "$HOME/.profile"
create_symlink "$DOTFILES_REPO/zprofile" "$HOME/.zprofile"


