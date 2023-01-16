{
  description = "A simple, quick and small window-manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        myGHCPackages = (hpkgs: with hpkgs; [
          X11
          ]);
      in
        rec {
          defaultPackage = packages.lul;
          packages.lul = pkgs.stdenv.mkDerivation {
            name = "lul";
            pname = "lul";
            version = "1.0";
            src = ./src;
          
            buildInputs = with pkgs; [
              (haskellPackages.ghcWithPackages myGHCPackages)
            ];
            # dontBuild = true;
            buildPhase = ''
              ghc Main.hs Lul.hs -o lul
            '';
            installPhase = ''
              mkdir -p $out/bin
              # ghc $src/Main.hs $src/Lul.hs -o lul
              cp lul $out/bin
            '';
          };
        }
      );
}
