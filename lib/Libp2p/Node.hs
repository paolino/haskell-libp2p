{- |
Module      : Libp2p.Node
Description : High-level node operations
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.Node
    ( -- * Node handle
      Node

      -- * Lifecycle
    , withNode

      -- * Operations
    , peerId
    , listen
    , dial
    ) where

import Control.Exception (bracket, throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString, withCString)
import Foreign.ForeignPtr
    ( ForeignPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.Ptr (Ptr, nullPtr)

import Libp2p.FFI qualified as FFI
import Libp2p.Types
    ( Libp2pError (..)
    , Multiaddr (..)
    , PeerId (..)
    )

-- | A managed libp2p node handle.
newtype Node = Node (ForeignPtr FFI.NodeHandle)

-- | Get the last error from the FFI layer.
getLastError :: IO Text
getLastError = do
    cstr <- FFI.c_last_error
    if cstr == nullPtr
        then pure "Unknown error"
        else T.pack <$> peekCString cstr

-- | Check return code and throw on failure.
checkRC :: IO Int -> IO ()
checkRC action = do
    rc <- fromIntegral <$> action
    if rc == 0
        then pure ()
        else do
            err <- getLastError
            throwIO $ userError $ T.unpack err

-- | Create a node, run an action, then free it.
withNode :: (Node -> IO a) -> IO a
withNode f = do
    raw <- FFI.c_node_new
    if raw == nullPtr
        then do
            err <- getLastError
            throwIO
                $ userError
                $ "Failed to create node: "
                <> T.unpack err
        else do
            fp <-
                newForeignPtr
                    ffiNodeFinalizer
                    raw
            f (Node fp)

foreign import ccall
    "& libp2p_node_free"
    ffiNodeFinalizer
        :: Foreign.ForeignPtr.FinalizerPtr FFI.NodeHandle

-- | Get the peer ID of a node.
peerId :: Node -> IO PeerId
peerId (Node fp) = withForeignPtr fp $ \ptr -> do
    cstr <- FFI.c_node_peer_id ptr
    if cstr == nullPtr
        then
            throwIO
                $ userError "Failed to get peer ID"
        else PeerId . T.pack <$> peekCString cstr

-- | Start listening on a multiaddress.
listen :: Node -> Multiaddr -> IO ()
listen (Node fp) (Multiaddr addr) =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack addr) $ \caddr ->
            checkRC $ FFI.c_node_listen ptr caddr

-- | Dial a remote peer at a multiaddress.
dial :: Node -> Multiaddr -> IO ()
dial (Node fp) (Multiaddr addr) =
    withForeignPtr fp $ \ptr ->
        withCString (T.unpack addr) $ \caddr ->
            checkRC $ FFI.c_node_dial ptr caddr
