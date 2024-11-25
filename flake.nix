{
  description = "Dev flake for IllogicApps";

  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs.url = "github:archevel/nixpkgs?ref=patch-1";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      includeVscode = true;
      includeRider = true;
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        vscodeNix =
          if includeVscode then
            import ./vscode.nix
          else
            {
              pkgs = _: [ ];
              unfreePackages = [ ];
            };
        riderNix =
          if includeRider then
            import ./rider.nix
          else
            {
              pkgs = _: [ ];
              unfreePackages = [ ];
            };
        unfreePackages = vscodeNix.unfreePackages ++ riderNix.unfreePackages;
        pkgs =
          if unfreePackages == [ ] then
            nixpkgs.legacyPackages.${system}
          else
            import nixpkgs {
              inherit system;
              config.allowUnfreePredicate = (path: builtins.elem (nixpkgs.lib.getName path) unfreePackages);
            };
        extraPackages = (vscodeNix.pkgs pkgs) ++ (riderNix.pkgs pkgs);
        myDotnetSdk = pkgs.dotnetCorePackages.sdk_8_0;
        myDotnetRuntime = pkgs.dotnetCorePackages.runtime_8_0;
        functionAppsDotnetSdk = pkgs.dotnetCorePackages.sdk_6_0;
        shellRequires = with pkgs; [
          (
            if includeVscode then
              (dotnetCorePackages.combinePackages [
                myDotnetSdk
                functionAppsDotnetSdk
              ])
            else
              myDotnetSdk
          )
        ];
      in
      {
        packages.default = pkgs.buildDotnetModule {
          pname = "IllogicApps";
          version = "0.0.1";
          src = ./.;
          nugetDeps = ./deps.nix;
          dotnet-sdk = myDotnetSdk;
          dotnet-runtime = myDotnetRuntime;
        };
        devShells.default = pkgs.mkShellNoCC {
          packages =
            with pkgs;
            [
              nuget-to-nix
            ]
            ++ extraPackages
            ++ shellRequires;
        };
      }
    );
}
