{pkgs, ...}: {
  services.printing = {
    enable = true;
    drivers = [pkgs.brlaser]; # Brother printer driver
  };
}
