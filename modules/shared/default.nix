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
    ];
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };
}
