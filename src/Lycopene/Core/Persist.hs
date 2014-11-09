{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Lycopene.Core.Persist
        ( connect
        ) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           System.FilePath
import           Lycopene.Configuration

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Project
  name String
  description String Maybe
  deriving Show
Release
  name String
  description String Maybe
  project ProjectId
  startOn Int
  endOn Int
Issue
  title String
  description String Maybe
  relrease ReleaseId
Record
  startOn Int
|]

-- type PersistM m a = SqlPersistT (NoLoggingT (ResourceT m)) a

{- | Run Persistent context with configuration.
-- 
runPersist :: Configuration -> SqlPersistT m a -> m a
runPersist c = runSqlite $ conn c where
  conn = (sqlDatabase . configureSqlite) c
  pool = createSqlitePool conn 3
-}

connect :: Configuration -> Text
connect = sqlDatabase . configureSqlite

configureSqlite :: Configuration -> SqliteConf
configureSqlite c = SqliteConf dataPath 3 where
  dataPath = (lycoHome c) </> "issues.db"
