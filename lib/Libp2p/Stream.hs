{- |
Module      : Libp2p.Stream
Description : High-level stream operations
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.Stream
    ( -- * Stream handle
      Stream (..)

      -- * Operations
    , readStream
    , writeStream
    , closeStream
    ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr
    ( ForeignPtr
    , withForeignPtr
    )
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)

import Libp2p.FFI qualified as FFI

-- | A managed libp2p stream handle.
newtype Stream = Stream (ForeignPtr FFI.StreamHandle)

-- | Get the last error from the FFI layer.
getLastError :: IO String
getLastError = do
    cstr <- FFI.c_last_error
    if cstr == nullPtr
        then pure "Unknown error"
        else peekCString cstr

{- | Read up to @n@ bytes from a stream.
Returns empty ByteString on EOF.
-}
readStream :: Stream -> Int -> IO ByteString
readStream (Stream fp) maxLen =
    withForeignPtr fp $ \ptr ->
        allocaBytes maxLen $ \buf -> do
            n <-
                fromIntegral
                    <$> FFI.c_stream_read
                        ptr
                        (castPtr buf)
                        (fromIntegral maxLen)
            if n < 0
                then do
                    err <- getLastError
                    throwIO $
                        userError $
                            "Stream read error: "
                                <> err
                else
                    BS.packCStringLen
                        (buf, fromIntegral n)

-- | Write bytes to a stream.
writeStream :: Stream -> ByteString -> IO ()
writeStream (Stream fp) bs =
    withForeignPtr fp $ \ptr ->
        unsafeUseAsCStringLen bs $ \(buf, len) -> do
            rc <-
                fromIntegral
                    <$> FFI.c_stream_write
                        ptr
                        (castPtr buf)
                        (fromIntegral len)
            if rc /= (0 :: Int)
                then do
                    err <- getLastError
                    throwIO $
                        userError $
                            "Stream write error: "
                                <> err
                else pure ()

-- | Close a stream (signal EOF).
closeStream :: Stream -> IO ()
closeStream (Stream fp) =
    withForeignPtr fp FFI.c_stream_close
