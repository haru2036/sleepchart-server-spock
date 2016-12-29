{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module SleepChart where

import Web.Spock
import Web.Spock.Config
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import           Model
import           Database.Persist.Sqlite(runSqlPool, insert, selectList, Entity, SelectOpt(..))
import           Config
import           Safe                        (readMay)
import           Data.Time
import           System.Environment          (lookupEnv)
import Data.Monoid
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Web.Scotty as S
import qualified Data.Text as T
import qualified Database.Persist.Sql as P
import           System.Environment          (lookupEnv)
import           Safe                        (readMay)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

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

