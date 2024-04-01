#!/usr/bin/env bash

echo Which languages would you like to install?
#!/bin/bash

echo "Do you want to install Rust? (Y/n)"
read answer
case ${answer,,} in
  n|no)
    echo "Rust installation skipped."
    ;;
  *)
    echo "Installing Rust..."
    # Add your installation command here
    mkdir -p ~/Toolbox
    RUSTUP_HOME=$RUSTUP_HOME CARGO_HOME=$CARGO_HOME bash -c 'curl https://sh.rustup.rs -sSf | sh'
    ;;
esac

echo "Do you want to install Haskell? (Y/n)"
read answer
case ${answer,,} in
  n|no)
    echo "Haskell installation skipped."
    ;;
  *)
    echo "Installing Haskell..."
    # Add your installation command here
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ;;
esac
