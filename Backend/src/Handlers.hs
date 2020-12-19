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


fetchChordsHandler :: Handler [Chord]
fetchChordsHandler = do
  liftIO $ putStrLn "chords"
  chords <- liftIO $ fetchChordsDB
  return chords


createChordHandler :: Chord -> Handler Int64
createChordHandler c = do
  liftIO $ putStrLn "create chord"
  uid <- liftIO $ createChordDB c
  return uid
