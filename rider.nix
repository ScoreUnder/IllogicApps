{
  unfreePackages = [
    "rider"
  ];
  pkgs = (
    pkgs: with pkgs; [
      jetbrains.rider
    ]
  );
}
