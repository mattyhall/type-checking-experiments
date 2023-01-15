{
  description = "HM in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:erikarvstedt/zls/fix-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, zig, zls, gitignore }:
    flake-utils.lib.eachSystem (builtins.attrNames zig.packages) (system:
      let
        pkgs = import nixpkgs { inherit system; };
        zigLatest = zig.packages.${system}.master;
        inherit (gitignore.lib) gitignoreSource;
      in
        rec {
          devShells.default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              zigLatest
              bashInteractive
              zls.packages.${system}.default
            ]);
          };
        }
    );
}
