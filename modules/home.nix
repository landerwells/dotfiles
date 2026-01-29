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
    };
  };

  home.packages = [];
  home.sessionVariables = {};
  programs.home-manager.enable = true;
}
