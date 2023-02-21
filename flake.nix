{
  description = "graph-view";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forallSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs { inherit system; };
    in {
      packages = forallSystems (system:
        let
          pkgs = nixpkgsFor system;
          haskellPackages = pkgs.haskellPackages;
        in rec {
          graph-view-server =
            haskellPackages.callCabal2nix "graph-view-server" ./server { };
          graph-view-web = pkgs.buildNpmPackage {
            pname = "graph-view-web";
            version = "0.1.0";
            src = ./web;
            npmDepsHash = "sha256-pWwYiNCHGKHzpWWCmHiisI/hJGxwndljDVqpkFRyyNo=";
            installPhase = ''
              mv dist $out
            '';
          };
          graph-view = pkgs.writeShellScriptBin "graph-view" ''
            ${graph-view-server}/bin/graph-view-server --webroot ${graph-view-web}
          '';
          default = graph-view;

        });
      devShells = forallSystems (system:
        let
          pkgs = nixpkgsFor system;
          haskellPackages = pkgs.haskellPackages;
          hls = pkgs.haskell-language-server.override { dynamic = true; };
        in {
          graph-view-server = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.graph-view-server ];
            buildInputs = [ haskellPackages.cabal-install hls ];
            withHoogle = true;
          };
          default = pkgs.mkShell {
            inputsFrom = [ self.devShells.${system}.graph-view-server ];
            nativeBuildInputs = with pkgs; [ nodejs ];
          };
        });
    };
}
