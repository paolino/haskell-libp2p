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
    find target/ -name 'libhaskell_libp2p.so' -o -name 'libhaskell_libp2p.dylib' \
      | grep -v deps \
      | head -1 \
      | xargs -I{} cp {} $out/lib/
    ls $out/lib/libhaskell_libp2p* || { echo "ERROR: shared library not found"; exit 1; }
  '';
}
