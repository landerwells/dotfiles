{
  config,
  pkgs,
  inputs,
  ...
}: let
  emacsOverlaySha256 = "06413w510jmld20i4lik9b36cfafm501864yq8k4vxl5r4hn0j0h";
in {
  nixpkgs = {
    overlays = [inputs.rust-overlay.overlays.default];
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };
}
