# Changelog

## 1.0.0 (2026-02-25)


### Features

* add listenAddrs API, fix test to dial before stream ([2cb2181](https://github.com/paolino/haskell-libp2p/commit/2cb21816bbad16e8f12ba9be8628a8e613166fa8))
* expose integration-tests as flake output ([7c7773b](https://github.com/paolino/haskell-libp2p/commit/7c7773b71f5b31c91c465332d1f7efa166d1db94))
* repo scaffold — Rust cdylib + Haskell FFI bindings ([fae867e](https://github.com/paolino/haskell-libp2p/commit/fae867eec6649a3ec2acb22c00fb1c07ea093987)), closes [#1](https://github.com/paolino/haskell-libp2p/issues/1)
* stream protocol handler + Haskell FFI bindings ([4d889a8](https://github.com/paolino/haskell-libp2p/commit/4d889a8c8d4fb967c387aee098b301ca2c2739a3))
* two-node echo integration test ([fc86d65](https://github.com/paolino/haskell-libp2p/commit/fc86d65e42fd8c501b431b0f6588dce488adeb5e))


### Bug Fixes

* add --enable-tests for fresh CI builds ([0182516](https://github.com/paolino/haskell-libp2p/commit/01825169f8879285d4572f477c2f531b7a3f5337))
* add FinalizerPtr import and use unqualified reference ([1b30970](https://github.com/paolino/haskell-libp2p/commit/1b309708e2304b63920b6bcbf5bcfa234925438f))
* apply cargo fmt formatting ([240c9ca](https://github.com/paolino/haskell-libp2p/commit/240c9ca6dce95ddd6b90aaefbfd68adfc9b5f4fb))
* apply fourmolu formatting ([bbabc28](https://github.com/paolino/haskell-libp2p/commit/bbabc286442d37356574471a521083f94b308e65))
* apply hlint suggestions (use when) ([5ed0036](https://github.com/paolino/haskell-libp2p/commit/5ed0036b57e677b26bb89e11237140d50d5e47b6))
* compilation errors and nix shell env vars ([261d659](https://github.com/paolino/haskell-libp2p/commit/261d65906d005e7ecb19d7d3fd857868ee79869b))
* find shared lib in target triple subdirectory ([ac3ec43](https://github.com/paolino/haskell-libp2p/commit/ac3ec43f404a102f08c493bf5a3ef2af35760755))
* pass extra-lib-dirs to cabal for Rust cdylib ([57b6890](https://github.com/paolino/haskell-libp2p/commit/57b68908e0a3fda3cdc3b23f5f73eeadc2a7881b))
* remove redundant async blocks (clippy) ([79a8b7d](https://github.com/paolino/haskell-libp2p/commit/79a8b7dd741bed78b9de137bc281f01b6b51c79a))
* revert to shellHook for env vars in nix shell ([f83aeab](https://github.com/paolino/haskell-libp2p/commit/f83aeaba25e09fa31109c7fe1ce008d57950c1a2))
* use overrideAttrs for shell env vars ([dd714da](https://github.com/paolino/haskell-libp2p/commit/dd714da71596aedc0f742d292e58ee286a98ac42))
