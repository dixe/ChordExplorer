{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module ApiType where

import Servant.API
import Data.Aeson
import Data.Int (Int64)
import GHC.Generics



--type ChordApi = FetchApi :<|> CreateApi


type ChordApi =
  "chords" :> Get '[JSON] [Chord]
  :<|> "chords" :> Capture "chordId" Int64 :> Get '[JSON] Chord
  :<|> "chords" :> ReqBody '[JSON] UploadChord :> Post '[JSON] Int64



data UploadChord = UploadChord {
  name :: String,
  svgBase64 :: String,
  tags :: [String]
  } deriving (Eq, Show, Generic)


data Chord = Chord {
  id :: Int64,
  name :: String,
  svgBase64 :: String,
  tags :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON UploadChord

instance FromJSON UploadChord

instance ToJSON Chord

instance FromJSON Chord
