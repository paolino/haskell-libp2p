# haskell-libp2p

Haskell bindings for [libp2p](https://libp2p.io/) via a Rust FFI
cdylib wrapping [rust-libp2p](https://github.com/libp2p/rust-libp2p).

## Features

- Node lifecycle (create, listen, dial)
- WebSocket transport with Noise encryption and Yamux muxing
- Stream-based custom protocols
- Safe Haskell API with `ForeignPtr` resource management

## Building

```bash
nix develop --quiet -c just build
```

## Testing

```bash
nix develop --quiet -c just test
```

## License

Apache-2.0
