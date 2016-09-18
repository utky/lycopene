{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Issue where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)
import qualified Lycopene.Database.Relational.Sprint as Sprint
import qualified Lycopene.Database.Relational.Project as Project
import qualified Lycopene.Core as Core

closeStatus :: Int
closeStatus = 0

openStatus :: Int
openStatus = 1

notReady :: Int
notReady = 0

ready :: Int
ready = 1

$(defineRelationFromDB "issue")
$(defineRelationFromDB "issue_status")

insertIssue' :: Core.SprintId -> Core.Issue -> InsertQuery ()
insertIssue' sp (Core.Issue i t d s) = insertQueryIssue encodeValues
  where
    encodeValues :: Relation () Issue
    encodeValues = relation $ do
      is <- query issue
      mx <- queryScalar $ aggregatedUnique issue issueNumber' max'
      let fmx = flattenMaybe mx
          one = just (value 1)
          next = fmx ?+? one
          defval = value 0
      return $
        Issue |$| value (Core.idStr i)
              |*| fromMaybe defval next
              |*| value t
              |*| value d
              |*| value (Core.idStr sp)
              |*| value (encodeStatus s)

encodeStatus :: Core.IssueStatus -> Int
encodeStatus Core.IssueOpen = 1
encodeStatus Core.IssueClosed = 0

selectBySprint :: Relation (String, Int) Issue
selectBySprint = relation' . placeholder $ \ph -> do
  i <- query issue
  wheres $ i ! sprintId' .=. ph ! fst'
  wheres $ i ! status' .=. ph ! snd'
  return i

toggleIssue :: Update (Int, String)
toggleIssue = typedUpdate tableOfIssue . updateTarget' $ \proj -> do
    fmap fst $ placeholder (\ph -> do
      status' <-# ph ! fst'
      wheres $ proj ! issueId' .=. ph ! snd')

openIssues :: Relation (String, Int) Issue
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
  return i
