{
  pkgs,
  inputs,
}:
with pkgs;
  [
    pkgconf
    poppler
    autoconf
    automake

    alejandra
    aspell
    aspellDicts.en
    bat
    bc
    btop
    claude-code
    # cmake
    coreutils
    fd
    ffmpeg_6
    fzf
    # gcc
    gh
    git
    gnumake
    hugo
    ispell
    dig
    killall
    # needed for vterm
    libtool
    glibtool
    lldb
    lua
    lua-language-server
    ninja
    nixd
    nodejs_24
    tmux
    openssh
    (python3.withPackages (ps:
      with ps; [
        networkx
        numpy
        scipy
        # add other packages you need
      ]))
    ripgrep
    emacsPackages.pdf-tools
    skim
    pass
    sqlite
    stow
    tokei
    typst
    unrar
    unzip
    wget
    zip
  ]
  ++ [
    # Add Neovim nightly from overlay
    inputs.neovim-nightly-overlay.packages.${pkgs.system}.default
  ]
