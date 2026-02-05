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
  home.homeDirectory = "/home/landerwells";
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
    };
  };

  home.packages = [];
  home.sessionVariables = {
    XCURSOR_PATH = "${config.home.homeDirectory}/dotfiles/cursors";
    PATH = "${config.home.homeDirectory}/.config/emacs/bin:$PATH";
    # export LESSHISTFILE=""
    # export MANPAGER='nvim +Man!'
    # export VISUAL=nvim
    # export EDITOR=nvim
    # export XDG_CACHE_HOME="$HOME/.cache"
    # export XDG_CONFIG_HOME="$HOME/.config"
  };
  programs.home-manager.enable = true;
}
