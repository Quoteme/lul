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
          text-format-simple
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
              ghc Main.hs Lul.hs Tree.hs Debug.hs -o lul
            '';
            installPhase = ''
              mkdir -p $out/bin
              # ghc $src/Main.hs $src/Lul.hs -o lul
              cp lul $out/bin
            '';
          };
          packages.lul-test = pkgs.stdenv.mkDerivation {
            name = "lul-test";
            src = ./test;
            version = "1.0";
            buildInputs = [
              packages.lul
              (pkgs.python310.withPackages (ps: with ps; [
                PyVirtualDisplay
              ]))
            ];
            dontBuild = true;
            installPhase = ''
            mkdir -p $out/bin
            cp $src/* $out/bin/
            ln -sf ${packages.lul}/bin/lul $out/bin/lul
            chmod +x $out/bin/lul-test.py
            mv $out/bin/lul-test.py $out/bin/lul-test
            '';
          };
        }
      );
}
