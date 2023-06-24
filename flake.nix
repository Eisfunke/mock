{
  description = "A GrEAt HAskeLL PrOGRaM to trANsForm tEXT.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    imports = [ inputs.haskell-flake.flakeModule ];
    perSystem = { self', pkgs, ... }: {
      haskellProjects.default = { };
      apps.default = self'.apps.mock;
      packages = {
        default = pkgs.haskell.lib.justStaticExecutables self'.packages.mock;
        image = pkgs.dockerTools.buildImage {
          name = "mock";
          tag = builtins.substring 0 9 (inputs.self.rev or "dev");
          config = {
            Cmd = [ "${self'.packages.default}/bin/mock" ];
          };
        };
        image-web = pkgs.dockerTools.buildImage {
          name = "mock-web";
          tag = builtins.substring 0 9 (inputs.self.rev or "dev");
          config = {
            Cmd = [ "${self'.packages.default}/bin/mock-web" ];
          };
        };
      };
    };
  };
}
