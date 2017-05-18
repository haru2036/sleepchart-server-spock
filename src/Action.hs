{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Action where

import Type
import Model 
import Model.ResponseTypes
import Utils
import Web.Spock hiding (get, SessionId)
import Data.HVect
import Control.Monad.IO.Class (liftIO)
import Data.Word8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Time.Clock
import qualified Crypto.Hash as Hash
import System.Random
import           Database.Persist.Sql
import           Database.Persist.Sqlite(runSqlPool, insert, selectList, Entity, SelectOpt(..))
import qualified Database.Persist as P 
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

data IsGuest = IsGuest
data IsUser = IsUser
data IsAdmin = IsAdmin

randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
      let (value, nextG) = next g
      in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g = BS.pack $ randomBytes len g

guestOnlyHook :: AppAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = maybeUser $ \mUser -> 
                do oldCtx <- getContext
                   case mUser of
                     Nothing -> return (IsGuest :&: oldCtx)
                     Just _ -> redirect "/"

hashPassword :: T.Text -> BS.ByteString -> BS.ByteString
hashPassword password salt =
       BSC8.pack $ show $ Hash.hashFinalize $ Hash.hashUpdates (Hash.hashInitWith Hash.SHA3_512) [salt, T.encodeUtf8 $ password]

registerUser :: T.Text -> T.Text -> T.Text -> SqlPersistM CommonResponse
registerUser name email password = do
  mUserNameU <- getBy (UniqueUsername name)
  mUserEmail <- getBy (UniqueEmail email)
  case (mUserNameU, mUserEmail) of
    (Just _ , _) -> 
      return $ CommonError "User name already taken"
    (_, Just _) -> 
      return $ CommonError "Email address already registered"
    (Nothing, Nothing) -> do
      g <- liftIO $ getStdGen
      let salt = randomBS 512 g
          hash = hashPassword password salt
      _ <- insert $ User name email (makeHex hash) (makeHex salt)
      return $ CommonSuccess "register completed"


loginUser :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
loginUser email password = do
  mUserEmail <- getBy $ UniqueEmail email
  case mUserEmail of
    Just entity -> do
      let user = entityVal entity
      return $ if userPassword user == (read $ show $ hashPassword password $ read $ show $ userSalt user) then Just $ entityKey entity
      else Nothing
    Nothing -> return Nothing

createSession :: UserId -> SqlPersistM SessionId
createSession userId = do
  now <- liftIO getCurrentTime
  insert (Session (addUTCTime (5 * 3600) now) userId)

killSessions :: UserId -> SqlPersistM ()
killSessions userId = deleteWhere [SessionUserId ==. userId]

maybeUser :: (Maybe (UserId, User) -> AppAction ctx a) -> AppAction ctx a
maybeUser action =
   do sess <- readSession
      case sess of
        Nothing ->
          action Nothing
        Just sid ->
          do mUser <- runSQL $ loadUser sid
             action mUser

loadUser :: Key Session -> SqlPersistM (Maybe (UserId, User))
loadUser sid = do
  mSess <- get sid
  now <- liftIO getCurrentTime
  case mSess of
    Just sess | sessionValidUntil sess > now ->
      do mUser <- get (sessionUserId sess)
         return $ fmap (\user -> (sessionUserId sess, user)) mUser
    _ -> return Nothing

