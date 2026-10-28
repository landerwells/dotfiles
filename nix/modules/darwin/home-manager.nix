{
  config,
  pkgs,
  lib,
  ...
}: let
  sharedFiles = import ../shared/files.nix {inherit config pkgs;};
  additionalFiles = import ./files.nix {inherit user config pkgs;};
in {
  imports = [
    ./dock
  ];

  # Fully declarative dock using the latest from Nix Store
  local.dock = {
    enable = true;
    username = user;
    entries = [
      {path = "/Applications/Slack.app/";}
      {path = "/System/Applications/Messages.app/";}
      {path = "/System/Applications/Facetime.app/";}
      {path = "${pkgs.alacritty}/Applications/Alacritty.app/";}
      {path = "/System/Applications/Music.app/";}
      {path = "/System/Applications/News.app/";}
      {path = "/System/Applications/Photos.app/";}
      {path = "/System/Applications/Photo Booth.app/";}
      {path = "/System/Applications/TV.app/";}
      {path = "/System/Applications/Home.app/";}
      {
        path = toString myEmacsLauncher;
        section = "others";
      }
      {
        path = "${config.users.users.${user}.home}/.local/share/";
        section = "others";
        options = "--sort name --view grid --display folder";
      }
      {
        path = "${config.users.users.${user}.home}/.local/share/downloads";
        section = "others";
        options = "--sort name --view grid --display stack";
      }
    ];
  };
}
