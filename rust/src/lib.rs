// haskell-libp2p: Rust cdylib wrapping rust-libp2p for Haskell FFI
//
// Architecture: One global tokio runtime (LazyLock). Each Node owns
// an mpsc::Sender<Command>. C FFI functions send commands and block
// on a oneshot reply channel.

#![allow(clippy::missing_safety_doc)]

use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::LazyLock;

use futures::StreamExt;
use libp2p::{
    identify, noise, tcp,
    swarm::{NetworkBehaviour, SwarmEvent},
    yamux, Multiaddr, Swarm, SwarmBuilder,
};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, oneshot};

/// Global tokio runtime shared by all nodes.
static RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    Runtime::new().expect("Failed to create tokio runtime")
});

// Thread-local last error message.
thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<CString>> =
        const { std::cell::RefCell::new(None) };
}

fn set_last_error(msg: &str) {
    LAST_ERROR.with(|e| {
        *e.borrow_mut() =
            Some(CString::new(msg).unwrap_or_default());
    });
}

// ── Behaviour ──────────────────────────────────────────────

#[derive(NetworkBehaviour)]
struct NodeBehaviour {
    identify: identify::Behaviour,
}

// ── Commands ───────────────────────────────────────────────

enum Command {
    Listen {
        addr: Multiaddr,
        reply: oneshot::Sender<Result<Multiaddr, String>>,
    },
    Dial {
        addr: Multiaddr,
        reply: oneshot::Sender<Result<(), String>>,
    },
}

// ── Node ───────────────────────────────────────────────────

/// Opaque node handle for C FFI.
pub struct LibP2PNode {
    peer_id: CString,
    cmd_tx: mpsc::Sender<Command>,
}

// ── Stream (placeholder for step 3) ───────────────────────

/// Opaque stream handle for C FFI.
pub struct LibP2PStream {
    _placeholder: u8,
}

// ── C API: Node lifecycle ──────────────────────────────────

/// Create a new libp2p node. Returns null on failure.
#[no_mangle]
pub extern "C" fn libp2p_node_new() -> *mut LibP2PNode {
    let result: Result<Box<LibP2PNode>, String> =
        RUNTIME.block_on(async {
            let swarm = SwarmBuilder::with_new_identity()
                .with_tokio()
                .with_tcp(
                    tcp::Config::default(),
                    noise::Config::new,
                    yamux::Config::default,
                )
                .map_err(|e| {
                    format!("Failed to build TCP: {e}")
                })?
                .with_websocket(
                    noise::Config::new,
                    yamux::Config::default,
                )
                .await
                .map_err(|e| {
                    format!("Failed to build WebSocket: {e}")
                })?
                .with_behaviour(|key| NodeBehaviour {
                    identify: identify::Behaviour::new(
                        identify::Config::new(
                            "/haskell-libp2p/0.1.0"
                                .to_string(),
                            key.public(),
                        ),
                    ),
                })
                .map_err(|e| {
                    format!("Failed to build behaviour: {e}")
                })?
                .build();

            let peer_id = *swarm.local_peer_id();
            let peer_id_str =
                CString::new(peer_id.to_string())
                    .map_err(|e| {
                        format!(
                            "Invalid peer ID string: {e}"
                        )
                    })?;

            let (cmd_tx, cmd_rx) = mpsc::channel(64);

            // Spawn the swarm event loop
            RUNTIME.spawn(run_swarm(swarm, cmd_rx));

            Ok(Box::new(LibP2PNode {
                peer_id: peer_id_str,
                cmd_tx,
            }))
        });

    match result {
        Ok(node) => Box::into_raw(node),
        Err(e) => {
            set_last_error(&e);
            std::ptr::null_mut()
        }
    }
}

/// Free a node.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_free(
    node: *mut LibP2PNode,
) {
    if !node.is_null() {
        drop(Box::from_raw(node));
    }
}

/// Get the PeerId as a null-terminated string.
/// Valid until the node is freed.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_peer_id(
    node: *const LibP2PNode,
) -> *const c_char {
    if node.is_null() {
        return std::ptr::null();
    }
    (*node).peer_id.as_ptr()
}

/// Start listening on the given multiaddr.
/// Returns 0 on success, -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_listen(
    node: *const LibP2PNode,
    multiaddr: *const c_char,
) -> i32 {
    if node.is_null() || multiaddr.is_null() {
        set_last_error("null pointer argument");
        return -1;
    }
    let addr_str = match CStr::from_ptr(multiaddr).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return -1;
        }
    };
    let addr: Multiaddr = match addr_str.parse() {
        Ok(a) => a,
        Err(e) => {
            set_last_error(&format!(
                "Invalid multiaddr: {e}"
            ));
            return -1;
        }
    };

    let (reply_tx, reply_rx) = oneshot::channel();
    let cmd = Command::Listen {
        addr,
        reply: reply_tx,
    };

    if (*node).cmd_tx.blocking_send(cmd).is_err() {
        set_last_error("Node event loop has stopped");
        return -1;
    }

    match RUNTIME.block_on(async { reply_rx.await }) {
        Ok(Ok(_actual_addr)) => 0,
        Ok(Err(e)) => {
            set_last_error(&e);
            -1
        }
        Err(_) => {
            set_last_error("Reply channel closed");
            -1
        }
    }
}

/// Dial a remote peer at the given multiaddr.
/// Returns 0 on success, -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_dial(
    node: *const LibP2PNode,
    multiaddr: *const c_char,
) -> i32 {
    if node.is_null() || multiaddr.is_null() {
        set_last_error("null pointer argument");
        return -1;
    }
    let addr_str = match CStr::from_ptr(multiaddr).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return -1;
        }
    };
    let addr: Multiaddr = match addr_str.parse() {
        Ok(a) => a,
        Err(e) => {
            set_last_error(&format!(
                "Invalid multiaddr: {e}"
            ));
            return -1;
        }
    };

    let (reply_tx, reply_rx) = oneshot::channel();
    let cmd = Command::Dial {
        addr,
        reply: reply_tx,
    };

    if (*node).cmd_tx.blocking_send(cmd).is_err() {
        set_last_error("Node event loop has stopped");
        return -1;
    }

    match RUNTIME.block_on(async { reply_rx.await }) {
        Ok(Ok(())) => 0,
        Ok(Err(e)) => {
            set_last_error(&e);
            -1
        }
        Err(_) => {
            set_last_error("Reply channel closed");
            -1
        }
    }
}

// ── C API: Streams (placeholder) ───────────────────────────

/// Read from stream. Returns bytes read, 0=EOF, -1=error.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_read(
    _stream: *mut LibP2PStream,
    _buf: *mut u8,
    _len: usize,
) -> i32 {
    set_last_error("Not yet implemented");
    -1
}

/// Write to stream. Returns 0=success, -1=error.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_write(
    _stream: *mut LibP2PStream,
    _buf: *const u8,
    _len: usize,
) -> i32 {
    set_last_error("Not yet implemented");
    -1
}

/// Close a stream.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_close(
    _stream: *mut LibP2PStream,
) {
}

/// Free a stream handle.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_free(
    stream: *mut LibP2PStream,
) {
    if !stream.is_null() {
        drop(Box::from_raw(stream));
    }
}

// ── C API: Error handling ──────────────────────────────────

/// Get the last error message. Returns null if no error.
#[no_mangle]
pub extern "C" fn libp2p_last_error() -> *const c_char {
    LAST_ERROR.with(|e| {
        e.borrow()
            .as_ref()
            .map_or(std::ptr::null(), |s| s.as_ptr())
    })
}

// ── Swarm event loop ───────────────────────────────────────

async fn run_swarm(
    mut swarm: Swarm<NodeBehaviour>,
    mut cmd_rx: mpsc::Receiver<Command>,
) {
    loop {
        tokio::select! {
            cmd = cmd_rx.recv() => {
                match cmd {
                    Some(Command::Listen { addr, reply }) => {
                        match swarm.listen_on(addr) {
                            Ok(_) => {
                                let _ = reply.send(Ok(
                                    Multiaddr::empty()
                                ));
                            }
                            Err(e) => {
                                let _ = reply.send(Err(
                                    format!(
                                        "Listen failed: {e}"
                                    )
                                ));
                            }
                        }
                    }
                    Some(Command::Dial { addr, reply }) => {
                        match swarm.dial(addr) {
                            Ok(()) => {
                                let _ = reply.send(Ok(()));
                            }
                            Err(e) => {
                                let _ = reply.send(Err(
                                    format!(
                                        "Dial failed: {e}"
                                    )
                                ));
                            }
                        }
                    }
                    None => break,
                }
            }
            event = swarm.select_next_some() => {
                match event {
                    SwarmEvent::NewListenAddr {
                        address,
                        ..
                    } => {
                        log::info!(
                            "Listening on {address}"
                        );
                    }
                    SwarmEvent::ConnectionEstablished {
                        peer_id,
                        ..
                    } => {
                        log::info!(
                            "Connected to {peer_id}"
                        );
                    }
                    SwarmEvent::ConnectionClosed {
                        peer_id,
                        ..
                    } => {
                        log::info!(
                            "Disconnected from {peer_id}"
                        );
                    }
                    _ => {}
                }
            }
        }
    }
}
