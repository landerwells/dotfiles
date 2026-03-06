{
  pkgs,
  inputs,
}:
with pkgs; let
  shared-packages = import ../shared/packages.nix {inherit pkgs inputs;};
in
  shared-packages
  ++ [
    # Desktop applications (that can't be shared easily with MacOS)
    blueberry
    direnv
    discord
    firefox
    flatpak
    font-manager
    fontconfig
    foot
    gcc
    gnuplot
    hyprlock
    hyprpicker
    hyprshot
    kdePackages.dolphin
    obs-studio
    obsidian
    pamixer
    pavucontrol
    perf
    racket-minimal
    rofi
    sioyek
    spotify
    swww
    unixtools.ifconfig
    unixtools.netstat
    valgrind
    virtualbox
    vlc
    waybar
    wiremix
    wireplumber
    wl-clipboard
    zathura
    zed-editor
    zotero
  ]
