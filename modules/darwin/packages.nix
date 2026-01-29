{
  pkgs,
  inputs,
}:
with pkgs; let
  shared-packages = import ../shared/packages.nix {inherit pkgs inputs;};
in
  shared-packages
  ++ [
    dockutil

    # put any other mac packages that I want
  ]
