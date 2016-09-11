{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Project where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)
import qualified Lycopene.Core as Core

$(defineRelationFromDB "project")

instance Eq Project where
  x == y = projectId x == projectId y

selectByName :: Relation String Project
selectByName = relation' . placeholder $ \ph -> do
  p <- query project
  wheres $ p ! name' .=. ph
  return p

insertProject' :: Core.Project -> InsertQuery ()
insertProject' (Core.Project i n d s) = insertQueryProject encodeValues
  where
    encodeValues :: Relation () Project
    encodeValues = relation . return $ 
      Project |$| value (Core.idStr i)
              |*| value n
              |*| value d
              |*| value (encodeStatus s)
    encodeStatus :: Core.ProjectStatus -> Int
    encodeStatus Core.ProjectInactive = 0
    encodeStatus Core.ProjectActive = 1

deleteById :: Core.ProjectId -> Delete ()
deleteById i =
  typedDelete tableOfProject . restriction $ \proj -> do
    wheres $ proj ! projectId' .=. value (Core.idStr i)
