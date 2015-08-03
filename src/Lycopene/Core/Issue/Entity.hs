{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Issue.Entity where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)
import qualified Lycopene.Core.Sprint.Entity as Sprint
import qualified Lycopene.Core.Project.Entity as Project

closeStatus :: Integer
closeStatus = 0

openStatus :: Integer
openStatus = 1

notReady :: Integer
notReady = 0

ready :: Integer
ready = 1

$(defineTable "issue")
$(defineTable "issue_status")

toggleIssue :: Update (Integer, Integer)
toggleIssue = typedUpdate tableOfIssue . updateTarget' $ \proj -> do
    fmap fst $ placeholder (\ph -> do
      status' <-# ph ! fst'
      wheres $ proj ! issueId' .=. ph ! snd')


-- | Row representation
data IssueR = IssueR
             { rIssueId :: Integer
             , rProjectName :: String
             , rSprintName :: String
             , rStatus :: String
             , rTitle :: String
             } deriving (Show)

instance Eq IssueR where
  x == y = rIssueId x == rIssueId y

$(makeRecordPersistableDefault ''IssueR)


openIssues :: Relation (Integer, Integer) IssueR
openIssues = relation' . placeholder $ \ph -> do
  i <- query issue
  s <- query Sprint.sprint
  p <- query Project.project
  st <- query issueStatus
  on $ i ! status'            .=. st ! statusId'
  on $ s ! Sprint.sprintId'   .=. i ! sprintId'
  on $ p ! Project.projectId' .=. s ! Sprint.projectId'
  wheres $ s ! Sprint.projectId' .=. ph ! fst'
  wheres $ i ! status'           .=. ph ! snd'
  return $ IssueR |$| i ! issueId'
                  |*| p ! Project.name'
                  |*| s ! Sprint.name'
                  |*| st ! statusName'
                  |*| i ! title'

data IssueV = IssueV
             { vTitle :: String
             , vDescription :: Maybe String
             , vSprintId :: Integer
             , vStatus :: Integer
             }

$(makeRecordPersistableDefault ''IssueV)


piIssueV :: Pi Issue IssueV
piIssueV = IssueV |$| title'
                  |*| description'
                  |*| sprintId'
                  |*| status'

insertIssueV :: Insert IssueV
insertIssueV = typedInsert tableOfIssue piIssueV

