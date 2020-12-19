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
import Database.Persist (selectList, (<.), SelectOpt(..), Entity, get, getBy, insert, selectList, entityVal, entityKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, fromSqlKey, toSqlKey)
import Data.Int (Int64)
import Data.Text (pack, unpack)
import Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)
import Data.Maybe (listToMaybe)

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


fetchChordDB' :: Int64 -> IO (Maybe (DbChord, [DbTag]))
fetchChordDB' cid = do
  maybeChord <- runAction localConnString selectAction
  (case maybeChord of
            Just chord -> do
              tags <- fetchTagsForChord cid
              return $ Just (chord, tags)
            Nothing -> return Nothing
         )
    where
      selectAction :: SqlPersistT (LoggingT IO) (Maybe DbChord)
      selectAction = ((fmap entityVal). listToMaybe)
                     <$> (select . from $ \chords -> do
                             where_ (chords ^. DbChordId ==. val (toSqlKey cid))
                             return chords)

fetchChordDB :: Int64 -> IO (Maybe Chord)
fetchChordDB cid = do
  maybeDbChord <- liftIO $ fetchChordDB' cid
  return (case maybeDbChord of
            Just (c,tags) -> Just $ fromDBChord cid c tags
            Nothing -> Nothing
            )


fetchTagsForChord :: Int64 -> IO ([DbTag])
fetchTagsForChord cid =  runAction localConnString selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) ([DbTag])
    selectAction = (fmap entityVal) <$>
                   (select . from $ \ (InnerJoin chord_to_tag tags)-> do
                       on (chord_to_tag ^. DbChordToTagTagId ==. tags ^. DbTagId)
                       where_ (chord_to_tag ^. DbChordToTagChordId ==. val (toSqlKey cid))
                       return tags)


fetchChordsDB' :: IO [Entity DbChord]
fetchChordsDB' = runAction localConnString (selectList [] [])

fetchChordsDB :: IO [Chord]
fetchChordsDB = do
  chords <- liftIO fetchChordsDB'
  return $ map fromDBChordEntity chords







createChordDB :: Chord -> IO Int64
createChordDB chord =
  let (dbChord, tags) = toDBChord chord
      cId = fromSqlKey <$> runAction localConnString (insert dbChord)
  in do
    cId <- fromSqlKey <$> runAction localConnString (insert dbChord)
    tagIds <- liftIO $ createTagsDB cId tags
    return cId



createTagsDB :: Int64 -> [DbTag] -> IO [Key DbTag]
createTagsDB cId tags = do
  ids <- mapM (\tag -> createOrGetTagId tag) tags
  createChordTagDBRelation cId ids


createChordTagDBRelation :: Int64 -> [Key DbTag] -> IO [Key DbTag]
createChordTagDBRelation cId tagids =
  let
    tagRelations = map (\tagKey -> DbChordToTag (toSqlKey cId) tagKey) tagids
  in
    do
     liftIO $ mapM (\rel -> runAction localConnString $ insert rel) tagRelations
     return tagids


createOrGetTagId :: DbTag -> IO (Key DbTag)
createOrGetTagId tag = do
  tid <- liftIO $ createGetTagByName tag
  case tid of
    Just entity -> return $ entityKey entity
    Nothing ->
      runAction localConnString $ insert tag


createGetTagByName :: DbTag -> IO (Maybe (Entity DbTag))
createGetTagByName (DbTag name) = runAction localConnString (getBy $ UniqueName name)




--DATA CONVERTERS


fromDBTag :: DbTag -> String
fromDBTag (DbTag name) = unpack name

fromDBChord :: Int64 -> DbChord -> [DbTag] -> Chord
fromDBChord id (DbChord name) tags = Chord id (unpack name) (map fromDBTag tags)

fromDBChordEntity :: Entity DbChord -> Chord
fromDBChordEntity e =
  let key = entityKey e
      chord = entityVal e
  in
    fromDBChord (fromSqlKey  key) chord []

toDBTag :: String -> DbTag
toDBTag tag = DbTag $ pack tag

toDBChord :: Chord -> (DbChord, [DbTag])
toDBChord (Chord id name tags) = (DbChord (pack name), map toDBTag tags)
