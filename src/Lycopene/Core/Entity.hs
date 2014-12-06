{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Lycopene.Core.Entity where

import           Data.Time
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project json
  name String
  description String Maybe
  UniqueProjectName name
  deriving Show

Sprint json
  name String
  description String Maybe
  project ProjectId
  startOn Int
  endOn Int
  deriving Show

Issue json
  title String
  description String Maybe
  sprint SprintId
  deriving Show

Record json
  issue IssueId
  startOn UTCTime
  endOn UTCTime
  deriving Show
  
|]

