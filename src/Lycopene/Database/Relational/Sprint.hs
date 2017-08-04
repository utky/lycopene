{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Sprint where

import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)
import qualified Lycopene.Database.Relational.Project as Pj
import qualified Lycopene.Core as Core

$(defineRelationFromDB "sprint")

insertSprint' :: Core.ProjectId -> Core.Sprint -> InsertQuery ()
insertSprint' p (Core.Sprint i n d s e st) = insertQuerySprint encodeValues
  where
    encodeValues :: Relation () Sprint
    encodeValues = relation . return $
      Sprint |$| value (Core.idStr i)
             |*| value n
             |*| value d
             |*| value (Core.idStr p)
             |*| value (fmap Core.toTime s)
             |*| value (fmap Core.toTime e)
             |*| value (encodeStatus st)

encodeStatus :: Core.SprintStatus -> Int
encodeStatus Core.SprintFinished = 0
encodeStatus Core.SprintRunning = 1

selectByProject :: Relation String Sprint
selectByProject = relation' . placeholder $ \ph -> do  
  s <- query sprint
  wheres $ s ! projectId' .=. ph
  return s


selectByProjectAndName :: Relation (String, String) Sprint
selectByProjectAndName = relation' . placeholder $ \ph -> do  
  s <- query sprint
  wheres $ s ! projectId' .=. ph ! fst'
  wheres $ s ! name' .=. ph ! snd'
  return s


selectByProjectAndStatus :: Relation (String, Int) Sprint
selectByProjectAndStatus = relation'. placeholder $ \ph -> do
  p <- query Pj.project
  s <- query sprint
  on $ p ! Pj.projectId' .=. s ! projectId'
  wheres $ p ! Pj.projectId' .=. ph ! fst'
  wheres $ s ! status' .=. ph ! snd'
  return s
