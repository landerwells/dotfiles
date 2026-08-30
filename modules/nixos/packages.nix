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
    anki
    blueman
    discord
    firefox
    flatpak
    font-manager
    fontconfig
    gcc
    ghostty
    gnuplot
    hyprpaper
    hyprpicker
    hyprshot
    libvterm
    mangohud
    obs-studio
    pamixer
    pavucontrol
    perf
    rofi
    spotify
    unixtools.ifconfig
    unixtools.netstat
    valgrind
    vlc
    waybar
    wiremix
    wireplumber
    wl-clipboard
    xwayland
    # xeyes
    xprop
    zathura
    zotero

    libGL
    vulkan-loader
    libx11
    libxcursor
    libxi
    libxrandr
    libxkbcommon
    wayland
    alsa-lib
  ]
