{
  description = "API for lunarbox";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    flake-utils.lib.eachSystem
      (with flake-utils.lib.system; [ x86_64-linux ])
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          easy-ps-nix = pkgs.callPackage easy-purescript-nix { };
        in
        {
          devShell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              easy-ps-nix.purs-0_13_6
              easy-ps-nix.spago
              nodejs
              yarn
              python3
            ];
          };
        });
}
