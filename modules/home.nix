{
  config,
  pkgs,
  lib,
  ...
}: let
  configDir = "${config.home.homeDirectory}/dotfiles/config";
  piAgentDir = "${configDir}/pi/agent";

  configFiles = builtins.listToAttrs (
    map (name: {
      name = ".config/${name}";
      value = {
        source = config.lib.file.mkOutOfStoreSymlink "${configDir}/${name}";
      };
    })
    (builtins.attrNames (builtins.readDir ../config))
  );

  piAgentFiles = {
    ".pi/agent/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${piAgentDir}/settings.json";
    ".pi/agent/extensions".source = config.lib.file.mkOutOfStoreSymlink "${piAgentDir}/extensions";
    ".pi/agent/themes".source = config.lib.file.mkOutOfStoreSymlink "${piAgentDir}/themes";
  };
in {
  home.stateVersion = "25.11";

  home.file = configFiles // piAgentFiles;

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

  xdg.desktopEntries = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    org-protocol = {
      name = "Org Protocol";
      comment = "Handle org-protocol:// URLs";
      exec = "emacsclient -- %u";
      icon = "emacs";
      type = "Application";
      terminal = false;
      categories = ["System"];
      mimeType = ["x-scheme-handler/org-protocol"];
      noDisplay = true;
    };
  };

  home.packages = [];
  home.sessionPath = [
    "${config.home.homeDirectory}/dotfiles/bin"
    "${config.home.homeDirectory}/dotfiles/bin/x86_64-linux"
  ];
  home.sessionVariables = {
    PI_SKIP_VERSION_CHECK = "1";
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
