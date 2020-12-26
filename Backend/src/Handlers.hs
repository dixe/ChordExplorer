{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)


import Data.Int (Int64)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List.Split

import Data.Maybe (catMaybes)

import Text.Read

import Servant
import ApiType
import Database


fetchChordHandler :: Int64 -> Handler Chord
fetchChordHandler cid = do
  _ <- liftIO $ putStrLn $"chord " ++ (show cid) -- basic IO operations with params
  maybeChord <- liftIO $ fetchChordDB cid
  case maybeChord of
    Just c -> return c
    Nothing -> Handler $ (throwE $ err401 { errBody = pack ("Could not find chord with ID: " ++ (show cid))})


fetchChordsHandler :: Maybe String -> Handler [Chord]
fetchChordsHandler queryIds =

  let
    ids = parseIds queryIds
  in
    do
      liftIO $ putStrLn "chords"
      chords <- liftIO $ fetchChordsDB ids
      return chords


parseIds :: Maybe String -> [Int64]
parseIds Nothing = []
parseIds (Just s) =
  let
    splitted = splitOn "," s
    mapped = map readMaybe splitted
  in
    catMaybes mapped






createChordHandler :: UploadChord -> Handler Id
createChordHandler c = do
  liftIO $ putStrLn "create chord"
  uid <- liftIO $ createChordDB c
  return $ Id uid
