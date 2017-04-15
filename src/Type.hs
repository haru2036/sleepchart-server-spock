
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeOperators, DataKinds,GADTs #-}
module Type where

import qualified Model as M
import qualified Database.Persist.Sqlite as PS hiding(get)
import Data.IORef
import Web.Spock 
import Web.Spock.Config


data MySession = Maybe M.SessionId 
data AppState = DummyAppState (IORef Int)
type SessionVal = Maybe M.SessionId
type AppAction ctx a = SpockActionCtx ctx PS.SqlBackend SessionVal AppState a
type App ctx a = SpockCtxM ctx PS.SqlBackend SessionVal AppState a

