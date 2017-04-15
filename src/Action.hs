{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Action where

import Type
import Model 
import Web.Spock hiding (get)
import Data.HVect
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import           Database.Persist.Sql
import           Database.Persist.Sqlite(runSqlPool, insert, selectList, Entity, SelectOpt(..))
import qualified Database.Persist as P 
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource

data IsGuest = IsGuest

guestOnlyHook :: AppAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = maybeUser $ \mUser -> 
                do oldCtx <- getContext
                   case mUser of
                     Nothing -> return (IsGuest :&: oldCtx)
                     Just _ -> redirect "/"

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


-- from funBlog
runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
      runQuery $ \conn ->
              runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

