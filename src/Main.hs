{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified JavaScript.WebSockets as WS

import qualified GHCJS.DOM as X
import qualified GHCJS.DOM.Document as X
import qualified GHCJS.DOM.Element as X
import qualified GHCJS.DOM.EventM as X
import qualified GHCJS.DOM.HTMLElement as X
import qualified GHCJS.DOM.HTMLParagraphElement as X
import qualified GHCJS.DOM.Node as X

import           ClientTypes

------------------------------------------------------------------------

sendRequest :: WS.Connection -> Cmd B.ByteString Int -> IO Bool
sendRequest conn cmd = WS.sendText conn (toText cmd)

receiveResponse :: WS.Connection -> IO (ClientResponse B.ByteString Int)
receiveResponse conn = fromText <$> WS.receiveText conn

main :: IO ()
main = run $ \conn webView -> do
    Just doc <- X.webViewGetDomDocument webView
    Just body <- X.documentGetBody doc
    X.htmlElementSetInnerHTML body ("<h1>Hello World</h1>" :: String)
    X.elementOnclick body $ do
      (x, y) <- X.mouseClientXY
      liftIO $ do
        X.htmlElementSetInnerHTML body ("<h1>Responses:</h1>" :: String)
        reqRsp conn doc body (CmdSet "x" x)
        reqRsp conn doc body (CmdSet "y" y)
        return ()
    forever (threadDelay 1000000 >> return ())
  where
    run io = X.runWebGUI $ \view ->
             WS.withUrl "ws://localhost:3000" $ \conn ->
             io conn view

    reqRsp conn doc body cmd = do
      addText doc body (show cmd)
      sendRequest conn cmd
      rsp <- receiveResponse conn
      addText doc body (show rsp)

    addText doc body str = do
      Just newParagraph <- fmap X.castToHTMLParagraphElement
                           <$> X.documentCreateElement doc ("p" :: String)
      X.htmlElementSetInnerText newParagraph str
      X.nodeAppendChild body (Just newParagraph)
