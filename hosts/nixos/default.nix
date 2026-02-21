{
  config,
  lib,
  inputs,
  pkgs,
  ...
}: let
  user = "landerwells";
  sshKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIqnqCb9HNWRQ2zZYaGFXJJ85W4IKKA9U0rci1A3dMNa"
  ];
in {
  imports = [
    ../../modules/shared
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 42;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ecbccafc-ad68-4400-8217-842f581f6bd3";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C449-E511";
    fsType = "vfat";
    options = ["fmask=0077" "dmask=0077"];
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/e4c1e732-6560-4742-b6da-ec24fa26134f";}
  ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  networking = {
    networkmanager.enable = true;
    hostName = "nixos";
    firewall = {
      enable = true;
      allowedTCPPorts = [53 3003]; # DNS over TCP + AdGuard Home web interface
      allowedUDPPorts = [53]; # DNS queries
      trustedInterfaces = ["tailscale0"]; # Trust Tailscale interface
    };
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Turn on flag for proprietary software
  nix = {
    nixPath = ["nixos-config=/home/${user}/.local/share/src/nixos-config:/etc/nixos"];
    settings = {
      allowed-users = ["${user}"];
      trusted-users = ["@admin" "${user}"];
      trusted-public-keys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="];
    };

    package = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  programs = {
    gnupg.agent.enable = true;
    # Needed for anything GTK related
    dconf.enable = true;
    zsh.enable = true;
    hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    steam = {
      enable = true;
      extraCompatPackages = [pkgs.proton-ge-bin];
    };
  };

  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1"; # If your cursor becomes invisible
    NIXOS_OZONE_WL = "1"; # Hint electron apps to use wayland
  };

  services = {
    adguardhome = {
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
            "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"
            "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"
          ];
      };
    };

    emacs = {
      enable = true;
      package = pkgs.emacs; # replace with emacs-gtk, or a version provided by the community overlay if desired.
      install = true;
      defaultEditor = true;
    };

    flatpak.enable = true;
    # Fallback console on tty1: auto-login your user
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = user;

    # Display manager & X server
    displayManager.sddm.enable = true;
    displayManager.sddm.wayland.enable = true;

    xserver = {
      enable = true;
      videoDrivers = ["nvidia"];

      # Uncomment for Nvidia GPU
      # This helps fix tearing of windows for Nvidia cards
      screenSection = ''
        Option       "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
        Option       "AllowIndirectGLXProtocol" "off"
        Option       "TripleBuffer" "on"
      '';

      xkb = {
        layout = "us";
        variant = "";
      };
    };

    # Better support for general peripherals
    libinput.enable = true;

    # Let's be able to SSH into this machine
    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        # AllowTcpForwarding = "yes";
      };
      allowSFTP = false;
    };

    tailscale = {
      enable = true;
      useRoutingFeatures = "server";
    };

    syncthing = {
      enable = true;
      user = "landerwells";
      group = "users";
      dataDir = "/home/landerwells/.local/share/syncthing"; # where database lives
      configDir = "/home/landerwells/.config/syncthing"; # where config lives
      openDefaultPorts = true;
      settings = {
        devices = {
          "macos" = {id = "JFSZU24-XA7JTXO-ZMZXO4L-KBDSOMA-2M74Y4X-GSO3EMF-YSMFDRR-AJG4XAW";};
          "hisense" = {id = "S4IKJUJ-NDJOT55-CHXXW4J-LKNWGSE-WATITGO-6YHWQZH-QI5TIXR-SBDFQQS";};
          "iphone" = {id = "VE4ZWNA-YKO33JS-65EPIYY-6X6BXFO-LLBSKJW-WBG5DQ2-BB5HRNK-Q5VM7AJ";};
        };
        folders = {
          "Books" = {
            path = "/home/landerwells/Books";
            devices = ["macos" "hisense"];
          };
          "dotfiles" = {
            path = "/home/landerwells/dotfiles";
            devices = ["macos"];
            ignorePerms = false; # Enable file permission syncing
          };
          "org" = {
            path = "/home/landerwells/org";
            devices = ["macos" "iphone"];
          };
        };
      };
    };

    # Enable CUPS to print documents
    printing.enable = true;
    printing.drivers = [pkgs.brlaser]; # Brother printer driver

    gvfs.enable = true; # Mount, trash, and other functionalities
    tumbler.enable = true; # Thumbnail support for images
  };

  # Configure Tailscale DNS settings
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

  # Video support
  hardware = {
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;

    enableAllFirmware = true;
    graphics.enable = true;

    # Enable Xbox support
    # hardware.xone.enable = true;

    nvidia = {
      modesetting.enable = true; # Enable modesetting required by newer desktops
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = true;
      nvidiaSettings = true;
    };
  };

  # It's me, it's you, it's everyone
  users.users = {
    ${user} = {
      isNormalUser = true;
      extraGroups = ["networkmanager" "wheel"];
      home = "/home/landerwells";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = sshKeys;
    };

    root = {
      openssh.authorizedKeys.keys = sshKeys;
    };
  };

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Don't require password for users in `wheel` group for these commands
  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "${pkgs.systemd}/bin/reboot";
            options = ["NOPASSWD"];
          }
          {
            command = "/run/current-system/sw/bin/nixos-rebuild";
            options = ["NOPASSWD"];
          }
        ];
        groups = ["wheel"];
      }
    ];
  };

  # Packages and Fonts
  environment.systemPackages = import ../../modules/nixos/packages.nix {inherit pkgs inputs;};
  fonts.packages = import ../../modules/shared/fonts.nix {inherit pkgs inputs;};

  xdg.portal.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];

  # # org-protocol desktop entry for capturing links from browser
  # environment.etc."xdg/applications/org-protocol.desktop".text = ''
  #   [Desktop Entry]
  #   Name=Org-Protocol
  #   Exec=emacsclient -n %u
  #   Icon=emacs
  #   Type=Application
  #   Terminal=false
  #   MimeType=x-scheme-handler/org-protocol
  # '';

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05";
}
