{...}: {
  services.syncthing = {
    enable = true;
    user = "landerwells";
    group = "users";
    dataDir = "/home/landerwells/.local/share/syncthing";
    configDir = "/home/landerwells/.config/syncthing";
    openDefaultPorts = true;
    settings = {
      devices = {
        "macos" = {id = "JFSZU24-XA7JTXO-ZMZXO4L-KBDSOMA-2M74Y4X-GSO3EMF-YSMFDRR-AJG4XAW";};
        "hisense" = {id = "S4IKJUJ-NDJOT55-CHXXW4J-LKNWGSE-WATITGO-6YHWQZH-QI5TIXR-SBDFQQS";};
        "fugu" = {id = "LTTABM2-KKXQKML-DHFHXLP-AOUGAG5-3UYTKPI-3RDHEYY-H5XZYNF-Q6LWXQT";};
      };
      folders = {
        "Books" = {
          path = "/home/landerwells/Books";
          devices = ["macos" "hisense"];
        };
        "dotfiles" = {
          path = "/home/landerwells/dotfiles";
          devices = ["macos"];
          ignorePerms = false;
        };
        "notes" = {
          path = "/home/landerwells/notes";
          devices = ["macos" "fugu"];
        };
      };
    };
  };
}
