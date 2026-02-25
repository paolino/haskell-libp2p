{- |
Module      : Libp2p.EchoSpec
Description : Two-node echo integration test
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Libp2p.EchoSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Test.Hspec (Spec, describe, it, shouldBe)

import Libp2p

spec :: Spec
spec = describe "Two-node echo" $ do
    it "exchanges messages over /test/echo/1.0.0" $ do
        withNode $ \nodeA -> do
            withNode $ \nodeB -> do
                -- Node A: listen on TCP + WS
                listen
                    nodeA
                    ( Multiaddr
                        "/ip4/127.0.0.1/tcp/0/ws"
                    )

                -- Register echo protocol on A
                registerProtocol
                    nodeA
                    "/test/echo/1.0.0"

                -- Wait for listener to bind
                threadDelay 500_000

                -- Get A's actual listen address
                addrsA <- listenAddrs nodeA
                addrsNE <-
                    case NE.nonEmpty addrsA of
                        Nothing ->
                            fail "No listen addresses"
                        Just ne -> pure ne
                let addrA = NE.head addrsNE

                -- Node B: dial Node A
                dial nodeB addrA

                -- Wait for connection
                threadDelay 500_000

                -- Node A: accept incoming stream
                -- (blocking, in separate thread)
                echoTask <- async $ do
                    stream <-
                        acceptStreamBlocking
                            nodeA
                            "/test/echo/1.0.0"
                    -- Read "hello"
                    msg <- readStream stream 1024
                    -- Write "world"
                    writeStream stream "world"
                    closeStream stream
                    pure msg

                -- Give protocol handler time
                threadDelay 200_000

                -- Node B: open stream to A
                pidA <- peerId nodeA
                streamB <-
                    openStream
                        nodeB
                        pidA
                        "/test/echo/1.0.0"

                -- B writes "hello"
                writeStream streamB "hello"

                -- B reads "world"
                response <- readStream streamB 1024
                closeStream streamB

                -- Check results
                received <- wait echoTask
                received `shouldBe` ("hello" :: ByteString)
                response `shouldBe` ("world" :: ByteString)
