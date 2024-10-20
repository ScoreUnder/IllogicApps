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
              version = "4.96.2";
              sha256 = "sha256-6WncLT4ooYPT2QQ8f0X41S0QMxuaL5ld0Cu3fvIisao=";
            }
            {
              name = "vscode-azurefunctions";
              publisher = "ms-azuretools";
              version = "1.15.4";
              sha256 = "sha256-k79SPlJR8nkvOoZ+8oZ9lMBes1qeSJgF0VLw8035zx0=";
            }
            {
              name = "vscode-azureresourcegroups";
              publisher = "ms-azuretools";
              version = "0.9.7";
              sha256 = "sha256-0V45NmIrpilyDmRgYIlO35ugmdXoNGIE5Ju6pm1kGpE=";
            }
            {
              name = "azurite";
              publisher = "azurite";
              version = "3.32.0";
              sha256 = "sha256-gYbsQ07jnRiaEhB2kdsPL8+ROIzaWZOT4bydU+iTZoA=";
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
