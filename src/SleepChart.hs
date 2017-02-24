{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeOperators, DataKinds,GADTs #-}
module SleepChart where

import Web.Spock
import Web.Spock.Config
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import           Model
import           Model.ResponseTypes
import           Text.Blaze.Html (Html, toHtml)
import           Database.Persist.Sqlite(runSqlPool, insert, selectList, Entity, SelectOpt(..))
import           Network.Wai.Middleware.Static
import           Config
import           Safe                        (readMay)
import           Data.Time
import           System.Environment          (lookupEnv)
import Data.Monoid
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Web.Scotty as S
import qualified Data.Text as T
import           Database.Persist.Sql         hiding(get)
import qualified Database.Persist.Sqlite as P hiding(get)
import           System.Environment          (lookupEnv)
import           Safe                        (readMay)
import           Data.HVect
import           Control.Monad.Reader.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Network.HTTP.Types.Status as Http


data MySession = Maybe SessionId 
data AppState = DummyAppState (IORef Int)
type SessionVal = Maybe SessionId
type AppAction ctx a = SpockActionCtx ctx P.SqlBackend SessionVal AppState a
type App ctx a = SpockCtxM ctx P.SqlBackend SessionVal AppState a

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

app :: App () ()
app = 
  prehook baseHook $ 
    do middleware (staticPolicy (addBase "static"))
       get root $
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
          now <- runSQL $ insert $ SleepSession now now 
          list <- runSQL $ selectList [] [P.Asc SleepSessionStart]
          json list

baseHook :: AppAction () (HVect '[])
baseHook = return HNil

-- from funBlog
runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
      runQuery $ \conn ->
              runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

