{ pkgs, rustLib }:
let
  indexState = "2026-02-01T00:00:00Z";
  project = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = {
      tools = {
        cabal = { index-state = indexState; };
        cabal-fmt = { index-state = indexState; };
        haskell-language-server = { index-state = indexState; };
        fourmolu = { index-state = indexState; };
        hlint = { index-state = indexState; };
      };
      buildInputs = with pkgs; [
        just
        nixfmt-classic
        cargo
        rustc
        rustfmt
        clippy
      ];
    };
    modules = [{
      packages.haskell-libp2p = {
        components = {
          library = {
            libs = pkgs.lib.mkForce [ (pkgs.lib.getLib rustLib) ];
            preBuild = ''
              export LD_LIBRARY_PATH="${rustLib}/lib:$LD_LIBRARY_PATH"
            '';
          };
          tests.integration-tests = {
            libs = pkgs.lib.mkForce [ (pkgs.lib.getLib rustLib) ];
            preBuild = ''
              export LD_LIBRARY_PATH="${rustLib}/lib:$LD_LIBRARY_PATH"
            '';
          };
        };
      };
    }];
  };
  flake = project.flake { };
in {
  packages = flake.packages // {
    integration-tests =
      project.hsPkgs.haskell-libp2p.components.tests.integration-tests;
  };
  devShells.default = project.shell.overrideAttrs (old: {
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ rustLib ];
    LIBRARY_PATH = pkgs.lib.makeLibraryPath [ rustLib ];
  });
}
