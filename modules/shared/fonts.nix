{
  pkgs,
  inputs,
  ...
}:
with pkgs; [
  nerd-fonts.fantasque-sans-mono
  nerd-fonts.gohufont
  nerd-fonts.fira-code
  nerd-fonts.jetbrains-mono
  nerd-fonts.roboto-mono

  # SF Mono from Lyndeno/apple-fonts.nix
  # inputs.apple-fonts.packages.${pkgs.system}.sf-mono-nerd
]
