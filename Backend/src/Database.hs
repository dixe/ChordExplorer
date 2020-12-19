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

import Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import Control.Monad.Reader (runReaderT, lift, liftIO)
import Control.Monad.IO.Class (MonadIO)
import Database.Persist (selectList, (==.), (<.), SelectOpt(..), Entity, get, insert)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, fromSqlKey, toSqlKey)
import Data.Int (Int64)

import DataConvert
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

selectChords :: (MonadIO m) => SqlPersistT m [Entity DbChord]
selectChords = selectList [] []


fetchChordDB' ::  Int64 -> IO (Maybe DbChord)
fetchChordDB' uid = do
  runAction localConnString (get (toSqlKey uid))



fetchChordDB :: Int64 -> IO (Maybe Chord)
fetchChordDB uid = do
  maybeChord <- liftIO $ fetchChordDB' uid
  case maybeChord of
    Just c -> return $ Just $ fromDBChord c
    Nothing -> return Nothing



createChordDB :: DbChord -> IO Int64
createChordDB chord = fromSqlKey <$> runAction localConnString (insert chord)
