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
          ignorePerms = false;
        };
        "org" = {
          path = "/home/landerwells/org";
          devices = ["macos" "iphone"];
        };
      };
    };
  };
}
