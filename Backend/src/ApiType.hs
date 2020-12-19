{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiType where

import Data.Text
import Servant.API
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Int (Int64)
import Data.Maybe
import Data.String.Conversions
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser



--type ChordApi = FetchApi :<|> CreateApi


type ChordApi =
  "chords" :> Get '[JSON] [Chord]
  :<|> "chords" :> Capture "chordId" Int64 :> Get '[JSON] Chord
  :<|> "chords" :> ReqBody '[JSON] Chord :> Post '[JSON] Int64

type CreateApi =
  "chords" :> ReqBody '[JSON] Chord :> Post '[JSON] Int64

data Chord = Chord {
  id :: Int64,
  name :: String,
  tags :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON Chord

instance FromJSON Chord


chords1 :: [Chord]
chords1 = [ Chord 1 "C" ["basic", "open"], Chord 2"E" ["basic", "open"]]
