{- |
Module      : Libp2p.Protocol
Description : Protocol registration and stream opening
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.Protocol
    ( -- * Protocol registration
      registerProtocol

      -- * Stream acquisition
    , acceptStream
    , acceptStreamBlocking
    , openStream
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text qualified as T
import Foreign.C.String (peekCString, withCString)
import Foreign.ForeignPtr
    ( FinalizerPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.Ptr (nullPtr)

import Libp2p.FFI qualified as FFI
import Libp2p.Node (Node (..))
import Libp2p.Stream (Stream (..))
import Libp2p.Types (PeerId (..))

-- | Get the last error from the FFI layer.
getLastError :: IO String
getLastError = do
    cstr <- FFI.c_last_error
    if cstr == nullPtr
        then pure "Unknown error"
        else peekCString cstr

{- | Register a protocol handler on a node.
After registration, incoming streams for this protocol
can be accepted with 'acceptStream' or
'acceptStreamBlocking'.
-}
registerProtocol :: Node -> T.Text -> IO ()
registerProtocol (Node fp) proto =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack proto) $ \cproto -> do
            rc <-
                fromIntegral
                    <$> FFI.c_node_register_protocol
                        ptr
                        cproto
            when (rc /= 0) $ do
                err <- getLastError
                throwIO $
                    userError $
                        "Failed to register protocol: "
                            <> err

{- | Non-blocking poll for an incoming stream.
Returns 'Nothing' if no stream is available.
-}
acceptStream :: Node -> T.Text -> IO (Maybe Stream)
acceptStream (Node fp) proto =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack proto) $ \cproto -> do
            raw <- FFI.c_node_accept_stream ptr cproto
            if raw == nullPtr
                then pure Nothing
                else do
                    sfp <-
                        newForeignPtr
                            ffiStreamFinalizer
                            raw
                    pure $ Just $ Stream sfp

-- | Blocking wait for an incoming stream.
acceptStreamBlocking :: Node -> T.Text -> IO Stream
acceptStreamBlocking (Node fp) proto =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack proto) $ \cproto -> do
            raw <-
                FFI.c_node_accept_stream_blocking
                    ptr
                    cproto
            if raw == nullPtr
                then do
                    err <- getLastError
                    throwIO $
                        userError $
                            "Failed to accept stream: "
                                <> err
                else do
                    sfp <-
                        newForeignPtr
                            ffiStreamFinalizer
                            raw
                    pure $ Stream sfp

-- | Open an outbound stream to a peer.
openStream :: Node -> PeerId -> T.Text -> IO Stream
openStream (Node fp) (PeerId pid) proto =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack pid) $ \cpid ->
            withCString (T.unpack proto) $ \cproto -> do
                raw <-
                    FFI.c_node_open_stream
                        ptr
                        cpid
                        cproto
                if raw == nullPtr
                    then do
                        err <- getLastError
                        throwIO $
                            userError $
                                "Failed to open stream: "
                                    <> err
                    else do
                        sfp <-
                            newForeignPtr
                                ffiStreamFinalizer
                                raw
                        pure $ Stream sfp

foreign import ccall "& libp2p_stream_free"
    ffiStreamFinalizer
        :: FinalizerPtr FFI.StreamHandle
