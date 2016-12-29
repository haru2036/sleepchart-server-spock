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
import           Data.Time


main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       doMigrations
       runSpock 8080 (spock spockCfg app)

