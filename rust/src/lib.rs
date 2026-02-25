// haskell-libp2p: Rust cdylib wrapping rust-libp2p for Haskell FFI
//
// Architecture: One global tokio runtime (LazyLock). Each Node owns
// an mpsc::Sender<Command>. C FFI functions send commands and block
// on a oneshot reply channel. Incoming protocol streams are pushed
// to per-protocol queues that Haskell polls from its own thread.

#![allow(clippy::missing_safety_doc)]

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::{LazyLock, Mutex};

use futures::io::{AsyncReadExt, AsyncWriteExt};
use futures::StreamExt;
use libp2p::{
    identify, noise,
    swarm::{NetworkBehaviour, SwarmEvent},
    tcp, yamux, Multiaddr, PeerId, Stream, StreamProtocol, Swarm, SwarmBuilder,
};
use libp2p_stream as stream;
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, oneshot};

/// Global tokio runtime shared by all nodes.
static RUNTIME: LazyLock<Runtime> =
    LazyLock::new(|| Runtime::new().expect("Failed to create tokio runtime"));

// Thread-local last error message.
thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<CString>> =
        const { std::cell::RefCell::new(None) };
}

fn set_last_error(msg: &str) {
    LAST_ERROR.with(|e| {
        *e.borrow_mut() = Some(CString::new(msg).unwrap_or_default());
    });
}

// ── Behaviour ──────────────────────────────────────────────

#[derive(NetworkBehaviour)]
struct NodeBehaviour {
    identify: identify::Behaviour,
    stream: stream::Behaviour,
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
    ListenAddrs {
        reply: oneshot::Sender<Vec<Multiaddr>>,
    },
}

// ── Stream wrapper ─────────────────────────────────────────

/// Opaque stream handle for C FFI.
/// Wraps a libp2p Stream with read/write via tokio channels.
pub struct LibP2PStream {
    /// Channel to request reads (sends back data).
    read_tx: mpsc::Sender<ReadRequest>,
    /// Channel to send write data.
    write_tx: mpsc::Sender<WriteRequest>,
}

struct ReadRequest {
    max_len: usize,
    reply: oneshot::Sender<Result<Vec<u8>, String>>,
}

struct WriteRequest {
    data: Vec<u8>,
    reply: oneshot::Sender<Result<(), String>>,
}

/// Spawn a task that bridges sync FFI calls to async stream I/O.
fn spawn_stream_bridge(stream: Stream) -> Box<LibP2PStream> {
    let (read_tx, mut read_rx) = mpsc::channel::<ReadRequest>(16);
    let (write_tx, mut write_rx) = mpsc::channel::<WriteRequest>(16);

    RUNTIME.spawn(async move {
        let (mut reader, mut writer) = stream.split();

        // Use select to handle both read and write requests
        loop {
            tokio::select! {
                req = read_rx.recv() => {
                    match req {
                        Some(ReadRequest {
                            max_len, reply
                        }) => {
                            let mut buf =
                                vec![0u8; max_len];
                            match reader
                                .read(&mut buf)
                                .await
                            {
                                Ok(0) => {
                                    let _ = reply
                                        .send(Ok(vec![]));
                                }
                                Ok(n) => {
                                    buf.truncate(n);
                                    let _ = reply
                                        .send(Ok(buf));
                                }
                                Err(e) => {
                                    let _ = reply.send(
                                        Err(format!(
                                            "Read error: \
                                             {e}"
                                        )),
                                    );
                                }
                            }
                        }
                        None => break,
                    }
                }
                req = write_rx.recv() => {
                    match req {
                        Some(WriteRequest {
                            data, reply
                        }) => {
                            match writer
                                .write_all(&data)
                                .await
                            {
                                Ok(()) => {
                                    let _ = writer
                                        .flush()
                                        .await;
                                    let _ =
                                        reply.send(Ok(()));
                                }
                                Err(e) => {
                                    let _ = reply.send(
                                        Err(format!(
                                            "Write error: \
                                             {e}"
                                        )),
                                    );
                                }
                            }
                        }
                        None => break,
                    }
                }
            }
        }
    });

    Box::new(LibP2PStream { read_tx, write_tx })
}

// ── Node ───────────────────────────────────────────────────

/// Opaque node handle for C FFI.
pub struct LibP2PNode {
    peer_id: CString,
    cmd_tx: mpsc::Sender<Command>,
    /// Stream control for opening outbound streams.
    control: Mutex<stream::Control>,
    /// Per-protocol incoming stream queues.
    incoming: Mutex<HashMap<String, mpsc::Receiver<Box<LibP2PStream>>>>,
}

// ── C API: Node lifecycle ──────────────────────────────────

/// Create a new libp2p node. Returns null on failure.
#[no_mangle]
pub extern "C" fn libp2p_node_new() -> *mut LibP2PNode {
    let result: Result<Box<LibP2PNode>, String> = RUNTIME.block_on(async {
        let stream_behaviour = stream::Behaviour::new();
        let control = stream_behaviour.new_control();

        let swarm = SwarmBuilder::with_new_identity()
            .with_tokio()
            .with_tcp(
                tcp::Config::default(),
                noise::Config::new,
                yamux::Config::default,
            )
            .map_err(|e| format!("Failed to build TCP: {e}"))?
            .with_websocket(noise::Config::new, yamux::Config::default)
            .await
            .map_err(|e| format!("Failed to build WebSocket: {e}"))?
            .with_behaviour(|key| NodeBehaviour {
                identify: identify::Behaviour::new(identify::Config::new(
                    "/haskell-libp2p/0.1.0".to_string(),
                    key.public(),
                )),
                stream: stream_behaviour,
            })
            .map_err(|e| format!("Failed to build behaviour: {e}"))?
            .build();

        let peer_id = *swarm.local_peer_id();
        let peer_id_str = CString::new(peer_id.to_string())
            .map_err(|e| format!("Invalid peer ID string: {e}"))?;

        let (cmd_tx, cmd_rx) = mpsc::channel(64);

        RUNTIME.spawn(run_swarm(swarm, cmd_rx));

        Ok(Box::new(LibP2PNode {
            peer_id: peer_id_str,
            cmd_tx,
            control: Mutex::new(control),
            incoming: Mutex::new(HashMap::new()),
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
pub unsafe extern "C" fn libp2p_node_free(node: *mut LibP2PNode) {
    if !node.is_null() {
        drop(Box::from_raw(node));
    }
}

/// Get the PeerId as a null-terminated string.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_peer_id(node: *const LibP2PNode) -> *const c_char {
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
            set_last_error(&format!("Invalid multiaddr: {e}"));
            return -1;
        }
    };

    let (reply_tx, reply_rx) = oneshot::channel();
    if (*node)
        .cmd_tx
        .blocking_send(Command::Listen {
            addr,
            reply: reply_tx,
        })
        .is_err()
    {
        set_last_error("Node event loop has stopped");
        return -1;
    }

    match RUNTIME.block_on(reply_rx) {
        Ok(Ok(_)) => 0,
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
            set_last_error(&format!("Invalid multiaddr: {e}"));
            return -1;
        }
    };

    let (reply_tx, reply_rx) = oneshot::channel();
    if (*node)
        .cmd_tx
        .blocking_send(Command::Dial {
            addr,
            reply: reply_tx,
        })
        .is_err()
    {
        set_last_error("Node event loop has stopped");
        return -1;
    }

    match RUNTIME.block_on(reply_rx) {
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

/// Get current listen addresses as newline-separated string.
/// Returns null on failure. Caller must free the returned
/// string with `libp2p_string_free`.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_listen_addrs(node: *const LibP2PNode) -> *mut c_char {
    if node.is_null() {
        set_last_error("null pointer argument");
        return std::ptr::null_mut();
    }

    let (reply_tx, reply_rx) = oneshot::channel();
    if (*node)
        .cmd_tx
        .blocking_send(Command::ListenAddrs { reply: reply_tx })
        .is_err()
    {
        set_last_error("Node event loop has stopped");
        return std::ptr::null_mut();
    }

    match RUNTIME.block_on(reply_rx) {
        Ok(addrs) => {
            let joined: String = addrs
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            match CString::new(joined) {
                Ok(cs) => cs.into_raw(),
                Err(e) => {
                    set_last_error(&format!("Invalid address string: {e}"));
                    std::ptr::null_mut()
                }
            }
        }
        Err(_) => {
            set_last_error("Reply channel closed");
            std::ptr::null_mut()
        }
    }
}

/// Free a string returned by `libp2p_node_listen_addrs`.
#[no_mangle]
pub unsafe extern "C" fn libp2p_string_free(s: *mut c_char) {
    if !s.is_null() {
        drop(CString::from_raw(s));
    }
}

// ── C API: Protocol registration ───────────────────────────

/// Register a protocol handler. Incoming streams for this
/// protocol are queued and can be polled with
/// `libp2p_node_accept_stream`.
/// Returns 0 on success, -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_register_protocol(
    node: *mut LibP2PNode,
    protocol_id: *const c_char,
) -> i32 {
    if node.is_null() || protocol_id.is_null() {
        set_last_error("null pointer argument");
        return -1;
    }
    let proto_str = match CStr::from_ptr(protocol_id).to_str() {
        Ok(s) => s.to_string(),
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return -1;
        }
    };

    let protocol = StreamProtocol::try_from_owned(proto_str.clone())
        .map_err(|e| format!("Invalid protocol: {e}"));
    let protocol = match protocol {
        Ok(p) => p,
        Err(e) => {
            set_last_error(&e);
            return -1;
        }
    };

    let node_ref = &*node;
    let mut control = node_ref.control.lock().unwrap();

    // Accept incoming streams for this protocol
    let mut incoming_streams = control.accept(protocol).unwrap();

    // Create a channel for Haskell to poll
    let (tx, rx) = mpsc::channel::<Box<LibP2PStream>>(64);

    node_ref
        .incoming
        .lock()
        .unwrap()
        .insert(proto_str.clone(), rx);

    // Spawn a task that accepts incoming streams and
    // wraps them for FFI
    RUNTIME.spawn(async move {
        while let Some((peer_id, stream)) = incoming_streams.next().await {
            log::info!(
                "Incoming stream from {peer_id} \
                 on {proto_str}"
            );
            let wrapped = spawn_stream_bridge(stream);
            if tx.send(wrapped).await.is_err() {
                break;
            }
        }
    });

    0
}

/// Poll for an incoming stream on the given protocol.
/// Returns null if no stream is available (non-blocking).
/// The caller must free the returned stream with
/// `libp2p_stream_free`.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_accept_stream(
    node: *mut LibP2PNode,
    protocol_id: *const c_char,
) -> *mut LibP2PStream {
    if node.is_null() || protocol_id.is_null() {
        set_last_error("null pointer argument");
        return std::ptr::null_mut();
    }
    let proto_str = match CStr::from_ptr(protocol_id).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return std::ptr::null_mut();
        }
    };

    let node_ref = &*node;
    let mut incoming = node_ref.incoming.lock().unwrap();
    if let Some(rx) = incoming.get_mut(proto_str) {
        match rx.try_recv() {
            Ok(stream) => Box::into_raw(stream),
            Err(mpsc::error::TryRecvError::Empty) => std::ptr::null_mut(),
            Err(mpsc::error::TryRecvError::Disconnected) => {
                set_last_error("Protocol handler disconnected");
                std::ptr::null_mut()
            }
        }
    } else {
        set_last_error("Protocol not registered");
        std::ptr::null_mut()
    }
}

/// Wait for an incoming stream (blocking).
/// Returns null on error.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_accept_stream_blocking(
    node: *mut LibP2PNode,
    protocol_id: *const c_char,
) -> *mut LibP2PStream {
    if node.is_null() || protocol_id.is_null() {
        set_last_error("null pointer argument");
        return std::ptr::null_mut();
    }
    let proto_str = match CStr::from_ptr(protocol_id).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return std::ptr::null_mut();
        }
    };

    let node_ref = &*node;
    let mut incoming = node_ref.incoming.lock().unwrap();
    if let Some(rx) = incoming.get_mut(proto_str) {
        match RUNTIME.block_on(async { rx.recv().await }) {
            Some(stream) => Box::into_raw(stream),
            None => {
                set_last_error("Protocol handler disconnected");
                std::ptr::null_mut()
            }
        }
    } else {
        set_last_error("Protocol not registered");
        std::ptr::null_mut()
    }
}

/// Open an outbound stream to a peer on a protocol.
/// Returns null on failure.
#[no_mangle]
pub unsafe extern "C" fn libp2p_node_open_stream(
    node: *mut LibP2PNode,
    peer_id: *const c_char,
    protocol_id: *const c_char,
) -> *mut LibP2PStream {
    if node.is_null() || peer_id.is_null() || protocol_id.is_null() {
        set_last_error("null pointer argument");
        return std::ptr::null_mut();
    }

    let peer_str = match CStr::from_ptr(peer_id).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return std::ptr::null_mut();
        }
    };
    let peer: PeerId = match peer_str.parse() {
        Ok(p) => p,
        Err(e) => {
            set_last_error(&format!("Invalid peer ID: {e}"));
            return std::ptr::null_mut();
        }
    };

    let proto_str = match CStr::from_ptr(protocol_id).to_str() {
        Ok(s) => s.to_string(),
        Err(e) => {
            set_last_error(&format!("Invalid UTF-8: {e}"));
            return std::ptr::null_mut();
        }
    };
    let protocol = match StreamProtocol::try_from_owned(proto_str) {
        Ok(p) => p,
        Err(e) => {
            set_last_error(&format!("Invalid protocol: {e}"));
            return std::ptr::null_mut();
        }
    };

    let node_ref = &*node;
    let mut control = node_ref.control.lock().unwrap();

    match RUNTIME.block_on(async { control.open_stream(peer, protocol).await }) {
        Ok(stream) => Box::into_raw(spawn_stream_bridge(stream)),
        Err(e) => {
            set_last_error(&format!("Failed to open stream: {e}"));
            std::ptr::null_mut()
        }
    }
}

// ── C API: Stream I/O ──────────────────────────────────────

/// Read up to `len` bytes from a stream into `buf`.
/// Returns number of bytes read, 0 on EOF, -1 on error.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_read(
    stream: *mut LibP2PStream,
    buf: *mut u8,
    len: usize,
) -> i32 {
    if stream.is_null() || buf.is_null() {
        set_last_error("null pointer argument");
        return -1;
    }

    let s = &*stream;
    let (reply_tx, reply_rx) = oneshot::channel();

    if s.read_tx
        .blocking_send(ReadRequest {
            max_len: len,
            reply: reply_tx,
        })
        .is_err()
    {
        return 0; // Stream closed = EOF
    }

    match RUNTIME.block_on(reply_rx) {
        Ok(Ok(data)) => {
            if data.is_empty() {
                return 0; // EOF
            }
            let n = data.len();
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf, n);
            n as i32
        }
        Ok(Err(e)) => {
            set_last_error(&e);
            -1
        }
        Err(_) => 0, // Channel closed = EOF
    }
}

/// Write `len` bytes from `buf` to a stream.
/// Returns 0 on success, -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_write(
    stream: *mut LibP2PStream,
    buf: *const u8,
    len: usize,
) -> i32 {
    if stream.is_null() || buf.is_null() {
        set_last_error("null pointer argument");
        return -1;
    }

    let s = &*stream;
    let data = std::slice::from_raw_parts(buf, len).to_vec();
    let (reply_tx, reply_rx) = oneshot::channel();

    if s.write_tx
        .blocking_send(WriteRequest {
            data,
            reply: reply_tx,
        })
        .is_err()
    {
        set_last_error("Stream write channel closed");
        return -1;
    }

    match RUNTIME.block_on(reply_rx) {
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

/// Close a stream (drop write side).
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_close(_stream: *mut LibP2PStream) {
    // Dropping the stream handle (via stream_free) closes
    // the channels which signals EOF to the bridge task.
}

/// Free a stream handle.
#[no_mangle]
pub unsafe extern "C" fn libp2p_stream_free(stream: *mut LibP2PStream) {
    if !stream.is_null() {
        drop(Box::from_raw(stream));
    }
}

// ── C API: Error handling ──────────────────────────────────

/// Get the last error message. Returns null if no error.
#[no_mangle]
pub extern "C" fn libp2p_last_error() -> *const c_char {
    LAST_ERROR.with(|e| e.borrow().as_ref().map_or(std::ptr::null(), |s| s.as_ptr()))
}

// ── Swarm event loop ───────────────────────────────────────

async fn run_swarm(mut swarm: Swarm<NodeBehaviour>, mut cmd_rx: mpsc::Receiver<Command>) {
    let mut listen_addrs: Vec<Multiaddr> = Vec::new();

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
                                        "Listen failed: \
                                         {e}"
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
                    Some(Command::ListenAddrs { reply }) => {
                        let _ = reply.send(
                            listen_addrs.clone()
                        );
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
                        listen_addrs.push(
                            address
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
