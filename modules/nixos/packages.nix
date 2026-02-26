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
    racket-minimal
    # alacritty
    anki
    blueberry
    discord
    firefox
    kdePackages.dolphin
    spotify
    vlc
    zathura
    valgrind
    perf
    zotero
    gnuplot

    font-manager
    fontconfig
    postgresql
    rofi
    swww
    unixtools.ifconfig
    unixtools.netstat
    waybar
    wl-clipboard
    # wayvnc
    # wayland
    # wayland-protocols
    # libxkbcommon
    # mesa
    # vulkan-loader

    impala
    wiremix
    pamixer
    pavucontrol
    wireplumber
    kitty
    ghostty

    obsidian
    sqlitebrowser

    direnv
    flatpak

    hyprlock
    hyprshot
    hyprpicker

    zed-editor

    gcc
  ]
