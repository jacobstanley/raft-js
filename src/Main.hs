{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified JavaScript.WebSockets as WS

import           ClientTypes

------------------------------------------------------------------------

sendRequest :: WS.Connection -> Cmd B.ByteString Int -> IO Bool
sendRequest conn cmd = WS.sendText conn (toText cmd)

receiveResponse :: WS.Connection -> IO (ClientResponse B.ByteString Int)
receiveResponse conn = fromText <$> WS.receiveText conn

main :: IO ()
main = WS.withUrl "ws://localhost:3000" $ \conn ->
  forever $ do
    sendRequest conn (CmdSet "x" 7)
    receiveResponse conn >>= print
    sendRequest conn (CmdSet "y" 8)
    receiveResponse conn >>= print
    sendRequest conn (CmdSet "x" 9)
    receiveResponse conn >>= print
    sendRequest conn (CmdSet "x" 2)
    receiveResponse conn >>= print
    sendRequest conn (CmdGet "x")
    receiveResponse conn >>= print
    threadDelay 1000000
