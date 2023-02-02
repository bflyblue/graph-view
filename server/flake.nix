{
  description = "graph-view-server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # hlib = pkgs.haskell.lib;
        # haskellPackages = pkgs.haskell.packages.ghc944.override {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: { };
        };
        hls = pkgs.haskell-language-server.override { dynamic = true; };
        packageName = "graph-view-server";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName ./. { };

        packages.default = self.packages.${system}.${packageName};

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.${packageName} ];
          buildInputs = [ haskellPackages.cabal-install hls ];
          withHoogle = true;
        };
      });
}
