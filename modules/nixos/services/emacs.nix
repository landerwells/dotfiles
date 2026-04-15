{pkgs, ...}: {
  services.emacs = {
    enable = true;
    startWithGraphical = false;
  };
}
