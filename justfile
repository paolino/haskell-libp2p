# Build the Rust cdylib
build-rust:
    cd rust && cargo build --release

# Build Haskell library (depends on Rust lib)
build: build-rust
    cabal build all -O0

# Run integration tests
test: build-rust
    cabal test integration-tests -O0 \
        --test-show-details=direct

# Format all sources
format:
    fourmolu -i lib test
    cd rust && cargo fmt
    nixfmt *.nix nix/*.nix

# Check formatting (no modification)
format-check:
    fourmolu --mode check lib test
    cd rust && cargo fmt -- --check

# Lint all sources
lint:
    hlint lib/ test/
    cd rust && cargo clippy -- -D warnings

# Full CI check
ci: format-check lint build test

# Clean all build artifacts
clean:
    cabal clean
    cd rust && cargo clean
