{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Config
import Model
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import           Data.Time
import qualified Data.Text as T
import qualified Database.Persist.Sql as P
import           System.Environment          (lookupEnv)
import           Safe                        (readMay)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       doMigrations
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       get "some-json" $ do
          now <- liftIO getCurrentTime
          json $ SleepSession now now 
       get "some-db" $ do
          now <- liftIO getCurrentTime
          liftIO $ runDb $ P.insert $ SleepSession now now 
          list <- liftIO $ runDb $ P.selectList [] [P.Asc SleepSessionStart]
          json list

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat
        [ "Failed to read [["
        , str
        , "]] for environment variable "
        , env
        ]
