{
  description = "graph-view";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    web = { url = "path:web"; };
    server = { url = "path:server"; };
  };

  outputs = { self, nixpkgs, flake-utils, web, server, ... }:
    let
      # pkgs = import nixpkgs { system = "x86_64-linux"; };
      supportedSystems = [ "x86_64-linux" ];
      forallSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forallSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forallSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          graph-view-server = server.packages.${system}.default;
          graph-view-web = pkgs.runCommandLocal "graph-view-web" { } ''
            mkdir -p "$out"
            cp -r ${web.packages.x86_64-linux.default}/lib/node_modules/graph-view/dist/* "$out"
          '';
        in {
          inherit graph-view-server graph-view-web;
          graph-view = pkgs.writeShellScriptBin "graph-view"
            "${graph-view-server}/bin/graph-view-server --webroot ${graph-view-web}";
          default = self.packages.${system}.graph-view;
        });
      devShells = forallSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            inputsFrom = [
              server.devShells.${system}.default
              web.devShells.${system}.default
            ];
          };
        });
    };
}
