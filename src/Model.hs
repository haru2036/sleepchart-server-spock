{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Time.Clock      (UTCTime)
import           Data.Time.Calendar   (Day)
import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           Database.Persist.Sqlite
import           Control.Monad.Trans.Resource
import           Control.Monad.Logger
import           GHC.Generics         (Generic)
import           Config



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name String
    email String
    deriving Show

SleepSession json
    start UTCTime
    end UTCTime 
    deriving Show

SleepInDay json
    date Day
    sleeps [SleepSession]
    deriving Show

|]

