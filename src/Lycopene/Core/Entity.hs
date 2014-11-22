{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
Project
  name String
  description String Maybe
  deriving Show
Sprint
  name String
  description String Maybe
  project ProjectId
  startOn Int
  endOn Int
Issue
  title String
  description String Maybe
  relrease SprintId
Record
  issue IssueId
  startOn UTCTime
  endOn UTCTime
|]

