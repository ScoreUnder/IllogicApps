{
  description = "Dev flake for IllogicApps";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        buildRequires = with pkgs;
          [ (with dotnetCorePackages; combinePackages [ sdk_6_0 sdk_8_0 ]) ];
      in {
        packages.default = pkgs.buildDotnetModule {
          pname = "IllogicApps";
          version = "0.0.1";
          src = ./.;
          nugetDeps = ./deps.nix;
          projectFile = "IllogicApps.Test/IllogicApps.Test.fsproj";
        };
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            nuget-to-nix
            dotnetPackages.Nuget
            azure-functions-core-tools
            nodejs
            azurite
          ];
          buildInputs = buildRequires;
        };
      });
}
