{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
    install = true;
    defaultEditor = true;
  };
}
