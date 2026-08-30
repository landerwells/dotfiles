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
        "fugu" = {id = "MXIZ437-CSW4RAW-FERBO63-OSRBEC6-2VDAFQK-GW22YGA-YWHLKB3-VOECXQV";};
      };
      folders = {
        "books" = {
          path = "/home/landerwells/books";
          devices = ["macos" "hisense" "fugu"];
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
