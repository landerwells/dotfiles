{
  config,
  pkgs,
  inputs,
  ...
}: {
  nixpkgs = {
    overlays = [
      inputs.rust-overlay.overlays.default
      inputs.emacs-overlay.overlays.default
      (import ../../overlays/pi-agent.nix)
    ];
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };
}
