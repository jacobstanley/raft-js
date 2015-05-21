{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -w #-}

module ClientTypes where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified JavaScript.WebSockets as WS

------------------------------------------------------------------------

data Cmd k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  | CmdHelp (Maybe B.ByteString)
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump
  deriving (Read, Show)

data ClientResponse k v =
    RspSetOK k v
  | RspGetOK k v
  | RspGetFail k
  deriving (Read, Show, Eq)

------------------------------------------------------------------------

class WebSocketsData a where
    fromText :: T.Text -> a
    toText   :: a -> T.Text

instance WebSocketsData (Cmd B.ByteString Int) where
    fromText = read . T.unpack
    toText   = T.pack . show

instance WebSocketsData (ClientResponse B.ByteString Int) where
    fromText = read . T.unpack
    toText   = T.pack . show
