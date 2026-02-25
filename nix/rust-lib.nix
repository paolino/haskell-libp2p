{ pkgs, rust-overlay }:
let
  rustPkgs = pkgs.extend rust-overlay.overlays.default;
  rustToolchain = rustPkgs.rust-bin.stable.latest.default;
in rustPkgs.rustPlatform.buildRustPackage {
  pname = "haskell-libp2p-rust";
  version = "0.1.0";
  src = ../rust;
  cargoLock.lockFile = ../rust/Cargo.lock;
  nativeBuildInputs = [ rustToolchain ];
  # Only install the shared library
  installPhase = ''
    mkdir -p $out/lib
    cp target/release/libhaskell_libp2p.so $out/lib/ \
      || cp target/release/libhaskell_libp2p.dylib $out/lib/ \
      || true
  '';
}
