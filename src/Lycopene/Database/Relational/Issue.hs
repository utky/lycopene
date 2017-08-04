{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Issue where

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

insertIssue' :: Core.ProjectId -> Core.Issue -> InsertQuery ()
insertIssue' pj (Core.Issue i t d s) = insertQueryIssue encodeValues
  where
    encodeValues :: Relation () Issue
    encodeValues = relation . return $
      Issue |$| value (Core.idStr i)
            |*| value t
            |*| value d
            |*| value (Core.idStr pj)
            |*| value (encodeStatus s)

encodeStatus :: Core.IssueStatus -> Int
encodeStatus Core.IssueOpen = 1
encodeStatus Core.IssueClosed = 0

selectBySprint :: Relation (String, Int) Issue
selectBySprint = relation' . placeholder $ \ph -> do
  i <- query issue
  s <- query Sprint.sprint
  on $ s ! Sprint.projectId' .=. i ! projectId'
  wheres $ s ! Sprint.sprintId' .=. ph ! fst'
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
  p <- query Project.project
  on $ p ! Project.projectId' .=. i ! projectId'
  wheres $ i ! projectId'     .=. ph ! fst'
  wheres $ i ! status'        .=. ph ! snd'
  return i

deleteById :: String -> Delete ()
deleteById i =
  typedDelete tableOfIssue . restriction $ \proj -> do
    wheres $ proj ! issueId' .=. value i
