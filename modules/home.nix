{
  config,
  pkgs,
  lib,
  ...
}: let
  configDir = "${config.home.homeDirectory}/dotfiles/config";

  configFiles = builtins.listToAttrs (
    map (name: {
      name = ".config/${name}";
      value = {
        source = config.lib.file.mkOutOfStoreSymlink "${configDir}/${name}";
      };
    })
    (builtins.attrNames (builtins.readDir ../config)) # Change this to ../config if modules/ and config/ are siblings
  );
in {
  home.stateVersion = "25.11";

  home.file = configFiles;

  programs = {
    git = {
      enable = true;
      settings.user.name = "landerwells";
      settings.user.email = "landerwells@gmail.com";
    };

    zsh = {
      enable = true;

      initContent = builtins.readFile ../config/zsh/zshrc;
      # profileExtra = builtins.readFile ../config/zsh/zprofile;
      # export LESSHISTFILE=""
      # export MANPAGER='nvim +Man!'
      # export PATH="$HOME/.config/emacs/bin:$PATH"
      # export VISUAL=nvim
      # export EDITOR=nvim
      # export XCURSOR_PATH="$HOME/dotfiles/cursors"
      # export XDG_CACHE_HOME="$HOME/.cache"
      # export XDG_CONFIG_HOME="$HOME/.config"
    };
  };

  home.packages = [];
  home.sessionVariables = {};
  programs.home-manager.enable = true;
}
