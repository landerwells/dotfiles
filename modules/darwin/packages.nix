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
    darwin.apple_sdk.frameworks.Cocoa

    # put any other mac packages that I want
  ]
