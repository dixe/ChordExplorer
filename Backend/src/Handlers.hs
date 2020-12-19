{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)


import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)

import Servant
import ApiType
import Database


fetchChordHandler :: Int64 -> Handler Chord
fetchChordHandler id = do
  liftIO $ putStrLn $"chord " ++ (show id) -- basic IO operations with params
  maybeChord <- liftIO $ fetchChordDB id
  case maybeChord of
    Just c -> return c
    Nothing -> Handler $ (throwE $ err401 { errBody = pack ("Could not find chord with ID: " ++ (show id))})


fetchChords :: Handler [Chord]
fetchChords = do
  liftIO $ putStrLn "chords"
  return chords1


createChord :: Chord -> Handler Int64
createChord c =
  return 1
