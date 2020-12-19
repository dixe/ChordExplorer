{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database where

import Prelude

import Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import Control.Monad.Reader (runReaderT, lift, liftIO)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist (selectList, (==.), (<.), SelectOpt(..), Entity, get, insert, selectList, entityVal, entityKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, fromSqlKey, toSqlKey)
import Data.Int (Int64)
import Data.Text (pack, unpack)

import Schema
import ApiType

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=chord_explorer password=admin"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)


fetchChordDB' ::  Int64 -> IO (Maybe DbChord)
fetchChordDB' uid = runAction localConnString (get (toSqlKey uid))

fetchChordDB :: Int64 -> IO (Maybe Chord)
fetchChordDB uid = do
  maybeChord <- liftIO $ fetchChordDB' uid
  case maybeChord of
    Just c -> return $ Just $ fromDBChord' uid c
    Nothing -> return Nothing


fetchChordsDB' :: IO [Entity DbChord]
fetchChordsDB' = runAction localConnString (selectList [] [])

fetchChordsDB :: IO [Chord]
fetchChordsDB = do
  chords <- liftIO fetchChordsDB'
  return $ map fromDBChord chords


createChordDB :: Chord -> IO Int64
createChordDB chord =
  let dbChord = toDBChord chord
  in fromSqlKey <$> runAction localConnString (insert dbChord)



--DATA CONVERTERS


fromDBChord' :: Int64 -> DbChord -> Chord
fromDBChord' id (DbChord name) = Chord id (unpack name) []

fromDBChord :: Entity DbChord -> Chord
fromDBChord e =
  let key = entityKey e
      chord = entityVal e
  in
    fromDBChord' (fromSqlKey  key) chord


toDBChord :: Chord -> DbChord
toDBChord (Chord id name tags) = DbChord (pack name)
