{- |
Module      : Libp2p.FFI
Description : Raw foreign imports for the libp2p C API
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.FFI
    ( -- * Opaque handles
      NodeHandle
    , StreamHandle

      -- * Node lifecycle
    , c_node_new
    , c_node_free
    , c_node_peer_id
    , c_node_listen
    , c_node_dial

      -- * Protocols
    , c_node_register_protocol
    , c_node_accept_stream
    , c_node_accept_stream_blocking
    , c_node_open_stream

      -- * Streams
    , c_stream_read
    , c_stream_write
    , c_stream_close
    , c_stream_free

      -- * Error handling
    , c_last_error
    ) where

import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

-- | Opaque handle for a libp2p node.
data NodeHandle

-- | Opaque handle for a libp2p stream.
data StreamHandle

-- Node lifecycle

foreign import ccall unsafe "libp2p_node_new"
    c_node_new :: IO (Ptr NodeHandle)

foreign import ccall unsafe "libp2p_node_free"
    c_node_free :: Ptr NodeHandle -> IO ()

foreign import ccall unsafe "libp2p_node_peer_id"
    c_node_peer_id
        :: Ptr NodeHandle -> IO CString

foreign import ccall safe "libp2p_node_listen"
    c_node_listen
        :: Ptr NodeHandle -> CString -> IO CInt

foreign import ccall safe "libp2p_node_dial"
    c_node_dial
        :: Ptr NodeHandle -> CString -> IO CInt

-- Protocols

foreign import ccall unsafe
    "libp2p_node_register_protocol"
    c_node_register_protocol
        :: Ptr NodeHandle -> CString -> IO CInt

foreign import ccall unsafe
    "libp2p_node_accept_stream"
    c_node_accept_stream
        :: Ptr NodeHandle
        -> CString
        -> IO (Ptr StreamHandle)

foreign import ccall safe
    "libp2p_node_accept_stream_blocking"
    c_node_accept_stream_blocking
        :: Ptr NodeHandle
        -> CString
        -> IO (Ptr StreamHandle)

foreign import ccall safe "libp2p_node_open_stream"
    c_node_open_stream
        :: Ptr NodeHandle
        -> CString
        -> CString
        -> IO (Ptr StreamHandle)

-- Streams

foreign import ccall safe "libp2p_stream_read"
    c_stream_read
        :: Ptr StreamHandle
        -> Ptr Word8
        -> CInt
        -> IO CInt

foreign import ccall safe "libp2p_stream_write"
    c_stream_write
        :: Ptr StreamHandle
        -> Ptr Word8
        -> CInt
        -> IO CInt

foreign import ccall unsafe "libp2p_stream_close"
    c_stream_close :: Ptr StreamHandle -> IO ()

foreign import ccall unsafe "libp2p_stream_free"
    c_stream_free :: Ptr StreamHandle -> IO ()

-- Error handling

foreign import ccall unsafe "libp2p_last_error"
    c_last_error :: IO CString
