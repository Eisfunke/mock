self:

{ pkgs, lib, config, ... }:

{
  options.services.mock-web = {
    enable = lib.mkEnableOption (lib.mdDoc "the Mock API");
    package = lib.mkOption {
      description = "The mock package to use.";
      type = lib.types.package;
      default = self.packages.${pkgs.hostPlatform.system}.mock;
    };
    port = lib.mkOption {
      description = "The port to use.";
      type = lib.types.port;
      default = 8080;
    };
  };

  config = let
    cfg = config.services.mock-web;
  in lib.mkIf cfg.enable {
    systemd.services.mock-web = {
      description = "Mock API";
      wantedBy = [ "multi-user.target" ];
      after = [ "networking.target" ];
      serviceConfig = {
        DynamicUser = true;
        RuntimeDirectory = "mock-web";
        WorkingDirectory = "%t/mock-web";
        Environment = [ "MOCK_WEB_PORT=${builtins.toString cfg.port}" ];
        ExecStart = "${cfg.package}/bin/mock-web";
        Restart = "always";
      };
    };
  };
}
