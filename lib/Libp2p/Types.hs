{- |
Module      : Libp2p.Types
Description : Core types for libp2p bindings
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.Types
    ( -- * Newtypes
      PeerId (..)
    , Multiaddr (..)

      -- * Errors
    , Libp2pError (..)
    ) where

import Data.Text (Text)

-- | A libp2p peer identifier.
newtype PeerId = PeerId {unPeerId :: Text}
    deriving stock (Eq, Ord, Show)

-- | A libp2p multiaddress.
newtype Multiaddr = Multiaddr {unMultiaddr :: Text}
    deriving stock (Eq, Ord, Show)

-- | Errors from the libp2p FFI layer.
newtype Libp2pError = Libp2pError {unLibp2pError :: Text}
    deriving stock (Eq, Show)
