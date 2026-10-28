{pkgs, ...}: {
  networking.firewall.trustedInterfaces = ["tailscale0"];

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
  };

  systemd.services.tailscale-dns-config = {
    description = "Configure Tailscale to accept DNS from network";
    after = ["tailscaled.service"];
    wants = ["tailscaled.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      # Wait for tailscale to be ready
      sleep 5
      # Configure Tailscale to not override local DNS and accept DNS from the network
      ${pkgs.tailscale}/bin/tailscale set --accept-dns=false
    '';
  };
}
