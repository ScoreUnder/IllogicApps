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
        myDotnetSdk = pkgs.dotnetCorePackages.sdk_8_0;
        myDotnetRuntime = pkgs.dotnetCorePackages.runtime_8_0;
        functionAppsDotnetSdk = pkgs.dotnetCorePackages.sdk_6_0;
        shellRequires = with pkgs;
          [ (dotnetCorePackages.combinePackages [ myDotnetSdk functionAppsDotnetSdk ]) ];
      in {
        packages.default = pkgs.buildDotnetModule {
          pname = "IllogicApps";
          version = "0.0.1";
          src = ./.;
          nugetDeps = ./deps.nix;
          projectFile = "IllogicApps.Test/IllogicApps.Test.fsproj";
          dotnet-sdk = myDotnetSdk;
          dotnet-runtime = myDotnetRuntime;
        };
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            nuget-to-nix
            dotnetPackages.Nuget
            azure-functions-core-tools
            nodejs
            azurite
          ];
          buildInputs = shellRequires;
        };
      });
}
