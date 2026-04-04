{
  pkgs,
  inputs,
}:
with pkgs; [
  aspell
  aspellDicts.en
  bat
  bc
  btop
  coreutils
  dig
  fd
  ffmpeg_6
  fzf
  gemini-cli
  gh
  git
  helix
  ispell
  killall
  nodejs_24
  openssh
  pass
  ripgrep
  skim
  tmux
  tree
  typst
  unrar
  unzip
  wget
  zip
  (rust-bin.stable.latest.default.override {
    extensions = ["rust-src" "rust-analyzer"];
  })
  neovim
]
