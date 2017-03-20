{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Project where

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


-- selectByName' :: Relation String Core.Project
-- selectByName' = relation' . placeholder $ \ph -> do
--   p <- query project
--   wheres $ p ! name' .=. ph
--   return $
--     Core.Project
--       |$| p ! projectId'
--       |*| p ! name'
--       |*| p ! description'
--       |*| p ! status'

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

deleteByName :: Core.Name -> Delete ()
deleteByName n =
  typedDelete tableOfProject . restriction $ \proj -> do
    wheres $ proj ! name' .=. value n
