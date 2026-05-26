{...}: {
  imports = [
    ./desktop.nix # Display manager, X server, XDG portal
    ./emacs.nix # Emacs daemon
    ./flatpak.nix # Flatpak
    ./miniflux.nix # RSS reader (web UI + sync API)
    ./ollama.nix # Local LLM server
    ./openssh.nix # SSH server
    ./pipewire.nix # Audio via PipeWire
    ./printing.nix # CUPS + Brother driver
    ./syncthing.nix # File sync
    ./tailscale.nix # VPN
  ];
}
