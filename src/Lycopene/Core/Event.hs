{-# LANGUAGE GADTs #-}
-- | Define request messages to communicate with domain.
module Lycopene.Core.Event where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Monad
import qualified Lycopene.Core.Project as Project
import qualified Lycopene.Core.Sprint as Sprint
import qualified Lycopene.Core.Issue as Issue


-- Use cases of Project
-- =======================================

-- | Aggregation of Project use-case
-- Lift ordinary values to Project semantices
data Event a where
  -- | Lift Name and Description into a volatile entity.
  NewProject :: Name -> Description -> Event Project.Project
  -- | Fetch a project which identified by ProjectId
  FetchProject :: Name -> Event Project.Project
  -- | Fetch list of all project regardless of its status.
  AllProject :: Event [Project.Project]
  -- | Remove a project by specified name from domain space.
  RemoveProject :: Name -> Event ()
  -- | Create a new sprint for the project.
  --NewSprint :: Name -> Description -> Maybe Date -> Maybe Date -> Project.ProjectId -> Event Sprint.SprintId
  -- | Fetch all sprint deriving from the project.
  FetchProjectSprint :: Name -> Event [Sprint.Sprint]
  -- | 
  FetchSprint :: Name -> Name -> Event Sprint.Sprint
  -- | 
  NewIssue :: Name -> Name -> String -> Event Issue.Issue
  -- | 
  FetchIssues :: Name -> Name -> Issue.IssueStatus -> Event [Issue.Issue]
  -- |
  FetchIssue :: Issue.IssueId -> Event Issue.Issue
  RemoveIssue :: Issue.IssueId -> Event ()

processEvent :: Event a -> Lycopene a
processEvent (NewProject n d) = do
  pj <- project $ Project.addProject $ Project.newProject n d
  _ <- sprint $ Sprint.newBacklog (Project.projectId pj)
  return pj

processEvent (RemoveProject n) =
  () <$ (project $ Project.removeProject n)

processEvent (FetchProject n) =
  project $ Project.fetchByNameProject n

processEvent AllProject =
  project $ Project.fetchAllProject

processEvent (FetchProjectSprint p) =
  sprint $ Sprint.fetchByStatusSprint p Sprint.SprintRunning

processEvent (FetchSprint p s) =
  sprint $ Sprint.fetchByNameSprint p s

processEvent (NewIssue pj sp t) = do
  parent <- sprint $ Sprint.fetchByNameSprint pj sp
  issue $ Issue.addIssue (Sprint.sprintId parent)
             $ Issue.newIssue t Nothing

processEvent (FetchIssues pj sp st) = do
  parent <- sprint $ Sprint.fetchByNameSprint pj sp
  issue $ Issue.fetchByStatusIssue (Sprint.sprintId parent) st

processEvent (FetchIssue is) = do
  issue $ Issue.fetchIssue is

processEvent (RemoveIssue is) = do
  issue $ Issue.removeIssue is
