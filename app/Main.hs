{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import SleepChart
import Config
import Model
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import Data.Time
import Control.Monad.Logger
import Database.Persist.Sqlite


main :: IO ()
main =
    do ref <- newIORef 0
       pool <- runNoLoggingT $ createSqlitePool ("test.sqlite3") 5
       runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
       spockCfg <- defaultSpockCfg Nothing (PCPool pool) (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

