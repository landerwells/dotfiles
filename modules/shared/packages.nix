{
  pkgs,
  inputs,
}:
with pkgs;
  [
    aspell
    aspellDicts.en
    bat
    bc
    btop
    claude-code
    coreutils
    fd
    ffmpeg_6
    fzf
    gh
    git
    gnumake
    hugo
    ispell
    dig
    killall
    libtool
    glibtool
    # lua
    # lua-language-server
    # ninja
    nodejs_24
    tmux
    openssh
    (python3.withPackages (ps:
      with ps; [
        networkx
        numpy
        scipy
        python-louvain
      ]))
    ripgrep
    skim
    pass
    nix-search-cli
    tokei
    typst
    unrar
    unzip
    wget
    tree
    zip
  ]
  ++ [
    # Add Neovim nightly from overlay
    inputs.neovim-nightly-overlay.packages.${pkgs.system}.default
  ]
