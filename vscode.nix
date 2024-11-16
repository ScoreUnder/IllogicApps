{
  unfreePackages = [
    "vscode"
    "vscode-with-extensions"
    "vscode-extension-github-copilot"
    "vscode-extension-github-copilot-chat"
  ];
  pkgs = (
    pkgs: with pkgs; [
      (vscode-with-extensions.override {
        vscodeExtensions =
          with vscode-extensions;
          [
            ms-dotnettools.csharp
            ms-dotnettools.vscode-dotnet-runtime
            ionide.ionide-fsharp
            github.copilot
            github.copilot-chat
          ]
          ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "vscode-azurelogicapps";
              publisher = "ms-azuretools";
              version = "4.107.4";
              sha256 = "sha256-Uk1uWZ7DY0E/gkaYj/PK59Ri6SvjrGV/maL1SuRW4p8=";
            }
            {
              name = "vscode-azurefunctions";
              publisher = "ms-azuretools";
              version = "1.16.1";
              sha256 = "sha256-Vvrpeg6QZAshzRCdzXXGYu2P3hrZVXwf9sZtB7ZX5jM=";
            }
            {
              name = "vscode-azureresourcegroups";
              publisher = "ms-azuretools";
              version = "0.9.9";
              sha256 = "sha256-f5jHcbUgN2SGaCDvejL8Nnu6IR+LRksNabG4YHZnJiQ=";
            }
            {
              name = "azurite";
              publisher = "azurite";
              version = "3.33.0";
              sha256 = "sha256-HnjamoLHQiqFQOpg0pY10CmHJF1b3+Lj2i3NxY/9QD0=";
            }
            {
              name = "azure-account";
              publisher = "ms-vscode";
              version = "0.12.0";
              sha256 = "sha256-AKvcxShSnGHWlwB8iWpcZYnCITRUP+vFLZ6naVAoI5s=";
            }
          ];
      })
      fantomas
      azure-functions-core-tools
      azurite
      nodejs
    ]
  );
}
