{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeOperators, DataKinds,GADTs #-}
module SleepChart where

import           Web.Spock
import           Web.Spock.Config
import           Type
import           Action
import           Utils
import qualified Web.Scotty as S
import           Data.Default
import           Data.Aeson (Value(..), object, (.=), FromJSON)
import           Network.Wai (Application)
import           Model hiding(SessionId)
import           Model.ResponseTypes
import           Text.Blaze.Html (Html, toHtml)
import           Network.Wai.Middleware.Static
import           Config
import           Safe                        (readMay)
import           Data.Time
import           System.Environment          (lookupEnv)
import           Data.Monoid
import           Data.IORef
import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sql         hiding(get)
import qualified Database.Persist as P 
import           System.Environment          (lookupEnv)
import           Safe                        (readMay)
import           Data.HVect
import           Data.Maybe
import qualified Data.Text as T
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import           Control.Monad
import qualified Network.HTTP.Types.Status as Http


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

       post "/sleep/start" $ do
         now <- liftIO getCurrentTime
         sleepingSession <- runSQL $ insert $ SleepingSession now $ dummyUser
         json sleepingSession

       post "/sleep/stop" $ do
         mSleeping <- runSQL $ P.getBy $ UniqueSleepingSession dummyUser 
         case mSleeping of
           Just sleeping -> do
             now <- liftIO getCurrentTime
             sleepSession <- runSQL $ do
               P.delete $ entityKey sleeping
               insert $ SleepSession now $ sleepingSessionStart $ entityVal sleeping
             json sleepSession
           Nothing -> setStatus Http.notFound404

       get "some-json" $ do
         now <- liftIO getCurrentTime
         json $ SleepSession now now 

       get "sleeps" $ do
         now <- liftIO getCurrentTime
         list <- runSQL $ selectList [] [P.Asc SleepSessionStart]
         json list
       post "/accounts/register" $ do
         mReqBody <- jsonBody
         case mReqBody of
           Just reqBody -> do
             result <- runSQL $ registerUser (sName reqBody) (sEmail reqBody) (sPassword reqBody) 
             json result
           Nothing -> json $ CommonError "invalid request"
       post "/accounts/login" $ do
         mReqBody <- jsonBody
         result <- runSQL $ runMaybeT $ login mReqBody
         setStatus $ fromMaybe Http.unauthorized401 $ join result

login :: Maybe LoginRequest -> MaybeT SqlPersistM (Maybe Http.Status) 
login (Just loginRequest) = do
     user <- MaybeT $ loginUser (lEmail loginRequest) (lPassword loginRequest) 
     _ <- lift $ createSession user 
     return $ Just Http.ok200
login Nothing = MaybeT $ return Nothing
  
dummyUser = User "hogehoge" "hoge@hoge.com" "hoge" "hoge"

baseHook :: AppAction () (HVect '[])
baseHook = return HNil

