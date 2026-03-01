{...}: {
  imports = [
    ./adguardhome.nix # DNS ad-blocking
    ./desktop.nix # Display manager, X server, XDG portal
    ./emacs.nix # Emacs daemon
    ./flatpak.nix # Flatpak
    ./openssh.nix # SSH server
    ./pipewire.nix # Audio via PipeWire
    ./printing.nix # CUPS + Brother driver
    ./syncthing.nix # File sync
    ./tailscale.nix # VPN
  ];
}
