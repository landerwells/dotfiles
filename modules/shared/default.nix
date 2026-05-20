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
      inputs.pi.overlays.default
      (final: prev: {
        pi-agent = prev.pi-coding-agent;
      })
    ];
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };
}
