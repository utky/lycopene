{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Project.Entity where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)

$(defineTable "project")

instance Eq Project where
  x == y = projectId x == projectId y

selectByName :: Relation String Project
selectByName = relation' . placeholder $ \ph -> do
  p <- query project
  wheres $ p ! name' .=. ph
  return p

data ProjectV = ProjectV
              { vName :: String
              , vDescription :: Maybe String
              }

$(makeRecordPersistableDefault ''ProjectV)

piProjectV :: Pi Project ProjectV
piProjectV = ProjectV |$| name'
                      |*| description'

insertProjectV :: Insert ProjectV
insertProjectV = typedInsert tableOfProject piProjectV
