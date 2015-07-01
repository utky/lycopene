{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Issue.Entity where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)
import qualified Lycopene.Core.Sprint.Entity as Sprint

$(defineTable "issue")
$(defineTable "issue_status")

-- | Row representation
data IssueR = IssueR
             { rIssueId :: Integer
             , rProjectName :: String
             , rSprintName :: String
             , rStatus :: String
             , rTitle :: String
             }

$(makeRecordPersistableDefault ''IssueR)


openIssues :: Relation (Integer, Integer) IssueR
openIssues = relation' . placeholder $ \ph -> do
  i <- query issue
  s <- query Sprint.sprint
  st <- query issueStatus
  on $ i ! status'          .=. st ! statusId'
  on $ s ! Sprint.sprintId' .=. i ! sprintId'
  wheres $ s ! Sprint.projectId' .=. ph ! fst'
  wheres $ i ! status'           .=. ph ! snd'
  -- FIXME: hard code
  return $ IssueR |$| i ! issueId'
                  |*| value "inbox"
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

closeStatus :: Integer
closeStatus = 0

openStatus :: Integer
openStatus = 1
