{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Sprint where

import           Data.Time (UTCTime)
import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)

$(defineRelationFromDB "sprint")

$(defineRelationFromDB "backlog_sprint")

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

selectBacklogByProject :: Relation String String
selectBacklogByProject = relation' . placeholder $ \ph -> do 
  a <- query backlogSprint
  wheres $ a ! backlogProjectId' .=. ph
  return $ a ! backlogSprintId'

data SprintV = SprintV
             { vName :: String
             , vDescription :: Maybe String
             , vProjectId :: String
             , vStartOn :: Maybe UTCTime 
             , vEndOn :: Maybe UTCTime}

$(makeRecordPersistableDefault ''SprintV)

piSprintV :: Pi Sprint SprintV
piSprintV = SprintV |$| name'
                    |*| description'
                    |*| projectId'
                    |*| startOn'
                    |*| endOn'

insertSprintV :: Insert SprintV
insertSprintV = typedInsert tableOfSprint piSprintV
