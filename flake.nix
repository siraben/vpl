{
  description = "zkeme80";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/854b5340529bef34fe98b8ed91eff95ee5bf03fd";
    utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };
  outputs = { self, nixpkgs, utils, flake-compat }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; rec {
        packages = rec {
          default = haskellPackages.callPackage ./project.nix { };
        };
        defaultPackage = self.packages.${system}.default;
      }
    );

}
