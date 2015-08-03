{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Sprint.Entity where

import           Data.Time.LocalTime
import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)

$(defineTable "sprint")

$(defineTable "backlog_sprint")

selectByProject :: Relation Integer Sprint
selectByProject = relation' . placeholder $ \ph -> do  
  s <- query sprint
  wheres $ s ! projectId' .=. ph
  return s


selectByProjectAndName :: Relation (Integer, String) Sprint
selectByProjectAndName = relation' . placeholder $ \ph -> do  
  s <- query sprint
  wheres $ s ! projectId' .=. ph ! fst'
  wheres $ s ! name' .=. ph ! snd'
  return s

selectBacklogByProject :: Relation Integer Integer
selectBacklogByProject = relation' . placeholder $ \ph -> do 
  a <- query backlogSprint
  wheres $ a ! backlogProjectId' .=. ph
  return $ a ! backlogSprintId'

data SprintV = SprintV
             { vName :: String
             , vDescription :: Maybe String
             , vProjectId :: Integer
             , vStartOn :: Maybe LocalTime
             , vEndOn :: Maybe LocalTime}

$(makeRecordPersistableDefault ''SprintV)

piSprintV :: Pi Sprint SprintV
piSprintV = SprintV |$| name'
                    |*| description'
                    |*| projectId'
                    |*| startOn'
                    |*| endOn'

insertSprintV :: Insert SprintV
insertSprintV = typedInsert tableOfSprint piSprintV
