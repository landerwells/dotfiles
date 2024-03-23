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

