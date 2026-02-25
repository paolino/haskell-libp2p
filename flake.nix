{
  description = "haskell-libp2p — Haskell bindings for libp2p via Rust FFI";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, rust-overlay, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
        };
        rustLib = import ./nix/rust-lib.nix { inherit pkgs rust-overlay; };
        project = import ./nix/project.nix { inherit pkgs rustLib; };
      in {
        packages = project.packages // { inherit rustLib; };
        devShells = project.devShells;
      });
}
