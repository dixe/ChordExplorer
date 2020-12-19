{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Int (Int64)
import Data.String.Conversions
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import ApiType
import Handlers


--SERVERS

server :: Server ChordApi
server = fetchChordsHandler
  :<|> fetchChordHandler
  :<|> createChordHandler


chordAPI :: Proxy ChordApi
chordAPI = Proxy

app1 :: Application
app1 = simpleCors $ serve chordAPI server
