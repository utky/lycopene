{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.BacklogSprint where

import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)
import qualified Lycopene.Database.Relational.Sprint as Sprint


$(defineRelationFromDB "backlog_sprint")

selectBacklogByProject :: Relation String Sprint.Sprint
selectBacklogByProject = relation' . placeholder $ \ph -> do 
  b <- query backlogSprint
  s <- query Sprint.sprint
  on $ s ! Sprint.sprintId' .=. b ! sprintId'
  wheres $ b ! projectId' .=. ph
  return $ s 
