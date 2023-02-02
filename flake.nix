{
  description = "graph-view";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    graph-view-web = { url = "./web"; };
    graph-view-server = { url = "./server"; };
  };

  outputs =
    { self, nixpkgs, flake-utils, graph-view-web, graph-view-server, ... }:
    let
      # pkgs = import nixpkgs { system = "x86_64-linux"; };
      supportedSystems = [ "x86_64-linux" ];
      forallSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forallSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forallSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          graph-view-server = graph-view-server.packages.${system}.default;
          graph-view-web = graph-view-web.packages.${system}.default;
          graph-view = pkgs.writeShellScriptBin "graph-view" ''
            ${self.packages.${system}.graph-view-server}/graph-view-server ${
              self.packages.${system}.graph-view-web
            }
          '';
          default = self.packages.${system}.graph-view;
        });
      devShells = forallSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            inputsFrom = [
              graph-view-server.devShells.${system}.default
              graph-view-web.devShells.${system}.default
            ];
          };
        });
    };
}
