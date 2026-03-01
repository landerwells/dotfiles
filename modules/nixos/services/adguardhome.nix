{...}: {
  networking.firewall = {
    allowedTCPPorts = [53 3003]; # DNS over TCP + AdGuard Home web interface
    allowedUDPPorts = [53]; # DNS queries
  };

  services.adguardhome = {
    enable = true;
    host = "0.0.0.0";
    port = 3003;
    settings = {
      dns = {
        bind_hosts = ["0.0.0.0"];
        port = 53;
        upstream_dns = [
          "9.9.9.9"
          "149.112.112.112"
        ];
      };
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
        parental_enabled = false;
        safe_search = {
          enabled = false;
        };
      };
      filters =
        map (url: {
          enabled = true;
          url = url;
        }) [
          # Ads
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_2.txt" # AdGuard DNS filter
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_6.txt" # EasyList
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_3.txt" # EasyPrivacy

          # Tracking & telemetry
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_4.txt" # Online Malicious URL Blocklist
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_31.txt" # Steven Black's hosts

          # Malware
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"
        ];
    };
  };
}
