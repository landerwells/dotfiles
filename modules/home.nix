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
    (builtins.attrNames (builtins.readDir ../config))
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
    };
  };

  home.packages = [];
  home.sessionPath = [
    "${config.home.homeDirectory}/dotfiles/bin"
    "${config.home.homeDirectory}/dotfiles/bin/x86_64-linux"
  ];
  home.sessionVariables = {
    XCURSOR_PATH = "${config.home.homeDirectory}/dotfiles/assets/cursors";
    PATH = "${config.home.homeDirectory}/.config/emacs/bin:${config.home.homeDirectory}/dotfiles/bin:$PATH";
    LESSHISTFILE = "";
    MANPAGER = "nvim +Man!";
    VISUAL = "nvim";
    EDITOR = "nvim";
    XDG_CACHE_HOME = "${config.home.homeDirectory}/.cache";
    XDG_CONFIG_HOME = "${config.home.homeDirectory}/.config";
  };
  programs.home-manager.enable = true;
}
