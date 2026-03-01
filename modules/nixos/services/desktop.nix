{pkgs, ...}: {
  services = {
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "landerwells";

    displayManager.sddm.enable = true;
    displayManager.sddm.wayland.enable = true;

    xserver = {
      enable = true;
      videoDrivers = ["nvidia"];

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

    libinput.enable = true;
    gvfs.enable = true; # Mount, trash, and other functionalities
    tumbler.enable = true; # Thumbnail support for images
  };

  xdg.portal.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
}
