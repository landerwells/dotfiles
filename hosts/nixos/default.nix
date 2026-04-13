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
    ../../modules/nixos/services
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
    firewall.enable = true;
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
    PATH = ["$HOME/dotfiles/bin"];
    LIBVA_DRIVER_NAME = "nvidia";
    XDG_SESSION_TYPE = "wayland";
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    WLR_RENDERER_ALLOW_SOFTWARE = "1";
  };

  # Video support
  hardware = {
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;

    enableAllFirmware = true;
    graphics.enable = true;

    nvidia = {
      modesetting.enable = true; # Enable modesetting required by newer desktops
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = true;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  # It's me, it's you, it's everyone
  users.users = {
    ${user} = {
      isNormalUser = true;
      extraGroups = ["networkmanager" "wheel" "audio" "vboxusers"];
      home = "/home/landerwells";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = sshKeys;
    };

    root = {
      openssh.authorizedKeys.keys = sshKeys;
    };
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

  virtualisation.virtualbox.host.enable = true;

  services.ollama = {
    enable = true;
    package = pkgs.ollama-vulkan; # ollama-cuda broken in current nixpkgs (cuda_compat-12.9 missing $src)
    loadModels = ["gemma4:12b"];
    environmentVariables = {
      OLLAMA_FLASH_ATTENTION = "1"; # Better VRAM efficiency on 4070
      OLLAMA_MAX_LOADED_MODELS = "1"; # Only keep one model loaded (12GB VRAM)
      OLLAMA_NUM_PARALLEL = "2"; # Parallel request slots
    };
  };

  system.stateVersion = "21.05";
}
