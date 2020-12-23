module Main where

import Server
import Network.Wai.Handler.Warp
import Control.Monad.Except
import Database (localConnString, migrateDB)
import Network.Wai.Logger (withStdoutLogger)

main :: IO ()
main =
  let port = 3000
  in
    do
      liftIO $ putStrLn $ "Running DB migrations"
      migrateDB localConnString
      liftIO $ putStrLn $ "Running on port: " ++ (show port)
      withStdoutLogger  $ \aplogger -> do
        let settings = setPort port $  setLogger aplogger defaultSettings
        runSettings settings app
