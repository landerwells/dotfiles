# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Nix flake providing both NixOS and nix-darwin system configurations, plus a shared home-manager profile, for user `landerwells`. Supports `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`, and `x86_64-darwin`.

## Common commands

- **Rebuild & switch (NixOS):** `~/dotfiles/bin/x86_64-linux/build-switch` (also on `$PATH` as `build-switch`). Runs `alejandra .` to format, `sudo nixos-rebuild switch --flake .#<arch>-linux`, then auto-commits with message `NixOS: Build generation N on DATE at TIME`.
- **Rebuild via flake app:** `nix run .#build-switch` (works on both Linux and Darwin; flake exposes per-platform apps in `flake.nix`: `apply`, `build-switch`, `copy-keys`, `create-keys`, `check-keys`, plus `build`/`rollback` on Darwin and `install` on Linux).
- **Format Nix files:** `alejandra .` (formatter the build script relies on; available in the dev shell).
- **Dev shell:** `nix develop` — provides `bashInteractive`, `git`, `nixd`, `alejandra`. `.envrc` contains `use flake`, so direnv auto-loads it.
- **Note:** `bin/rebuild` is stale (points to a nonexistent `~/dotfiles/nixos-config/`). Use `build-switch` instead.

## Architecture

Entry point is `flake.nix`. It builds:

- `darwinConfigurations.<arch>-darwin` → imports `./hosts/darwin/default.nix`, wires in `nix-homebrew` (taps for core/cask/bundle/emacsmacport) and `home-manager` pointing at `./modules/home.nix`.
- `nixosConfigurations.<arch>-linux` → imports `./hosts/nixos/default.nix`, with `home-manager` pointing at `./modules/home.nix`.

Layout:

- `hosts/{darwin,nixos}/default.nix` — per-platform system config (hardware, bootloader, users, host-specific services like Ollama/Hyprland/Steam on NixOS; homebrew + system defaults on Darwin).
- `modules/shared/` — imported by both hosts. `default.nix` sets up overlays (`rust-overlay`, `emacs-overlay`) and nixpkgs config (`allowUnfree`, `allowBroken`). `packages.nix` and `fonts.nix` are lists shared across platforms.
- `modules/nixos/packages.nix` — NixOS-only packages, extends `modules/shared/packages.nix`.
- `modules/nixos/services/` — one file per service (adguardhome, syncthing, tailscale, pipewire, emacs daemon, etc.). `default.nix` is an import index: **add/remove a service by creating/deleting its file AND updating the `imports` list in `services/default.nix`**.
- `modules/darwin/` — `casks.nix` (homebrew casks), `packages.nix`, `home-manager.nix` (dock entries), `files.nix`.
- `modules/home.nix` — home-manager profile used by both NixOS and Darwin. Configures git/zsh, session paths, and **symlinks every subdirectory of `./config/` into `~/.config/<name>` via `mkOutOfStoreSymlink`**. Those config files are therefore live-editable without a rebuild.
- `config/` — non-Nix dotfiles (doom, alacritty, ghostty, hypr, waybar, zsh, nvim, tmux, etc.). Edit these directly; no rebuild needed because they're symlinked out-of-store.
- `bin/` — helper scripts. Platform-specific subdirs (`bin/x86_64-linux/`, `bin/aarch64-darwin/`, …) are invoked by the flake's `apps`. `bin/apps/` and top-level scripts (e.g. `omarchy-launch-*`, `tmux-*`) are on `$PATH` via `home.sessionPath`.
- `plugins/` — git submodules (zsh-autosuggestions, zsh-autopair, zsh-syntax-highlighting, pomodoro). Run `git submodule update --init --recursive` after cloning.

## Conventions

- Nix files are formatted with **alejandra**; `build-switch` runs it automatically before rebuilding, so stray formatting diffs will be committed alongside real changes.
- `build-switch` auto-`git add -A` and commits after every successful rebuild — expect a commit per invocation.
- NixOS passwordless sudo is configured only for `nixos-rebuild` and `reboot` (see `hosts/nixos/default.nix`).
- `nixpkgs` follows `nixos-unstable`; `nixpkgs-latest` (master) is used narrowly (currently for `ollama-vulkan` via an overlay in `hosts/nixos/default.nix`).
