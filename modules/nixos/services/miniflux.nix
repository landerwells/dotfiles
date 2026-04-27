{pkgs, ...}: {
  services.postgresql.package = pkgs.postgresql_16;

  services.miniflux = {
    enable = true;
    adminCredentialsFile = "/var/lib/miniflux/admin-credentials";
    config = {
      LISTEN_ADDR = "0.0.0.0:8080";
      BASE_URL = "http://localhost:8080/";
    };
  };
}
