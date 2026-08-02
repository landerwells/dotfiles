{
  config,
  pkgs,
  inputs,
  ...
}: let
  user = "landerwells";
in {
  imports = [
    ../../modules/shared
  ];

  nix = {
    package = pkgs.nix;

    # It me
    # users.users.${user} = {
    #   name = "${user}";
    #   home = "/Users/${user}";
    #   isHidden = false;
    #   shell = pkgs.zsh;
    # };

    settings = {
      trusted-users = ["@admin" "${user}"];
    };

    gc = {
      automatic = true;
      interval = {
        Weekday = 0;
        Hour = 2;
        Minute = 0;
      };
      options = "--delete-older-than 30d";
    };

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  environment.systemPackages = import ../../modules/darwin/packages.nix {inherit pkgs inputs;};
  fonts.packages = import ../../modules/shared/fonts.nix {inherit pkgs inputs;};

  homebrew = {
    enable = true;
    casks = pkgs.callPackage ../../modules/darwin/casks.nix {};
    # cleanup left unset (not "uninstall"): running `brew bundle`'s install and
    # cleanup together in one invocation (which is what onActivation.cleanup
    # triggers) hits a Homebrew bug where its cask-name cache gets stale and it
    # wrongly thinks already-installed casks aren't declared, so it tries to
    # uninstall them all. Run `brew bundle cleanup --force` by hand instead if
    # you actually want to prune untracked casks.
    onActivation.extraEnv = {
      HOMEBREW_BUNDLE_ADOPT = "1";
    };

    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    #
    # If you have previously added these apps to your Mac App Store profile (but not installed them on this system),
    # you may receive an error message "Redownload Unavailable with This Apple ID".
    # This message is safe to ignore. (https://github.com/dustinlyons/nixos-config/issues/83)
  };

  system = {
    checks.verifyNixPath = false;
    primaryUser = user;
    stateVersion = 5;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

      NSGlobalDomain = {
        AppleICUForce24HourTime = false;
        ApplePressAndHoldEnabled = false;
        AppleShowAllExtensions = true;
        NSWindowShouldDragOnGesture = true;

        # KeyRepeat = 2; # Values: 120, 90, 60, 30, 12, 6, 2
        # InitialKeyRepeat = 15; # Values: 120, 94, 68, 35, 25, 15

        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = true;
        show-recents = false;
        launchanim = true;
        orientation = "bottom";
        tilesize = 32;
        mru-spaces = false;
      };

      finder = {
        _FXShowPosixPathInTitle = false;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };
  };
}
