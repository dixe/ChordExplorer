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
import Network.Wai.Middleware.Cors

import Servant
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
