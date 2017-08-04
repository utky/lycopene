{-# LANGUAGE GADTs #-}
-- | Define request messages to communicate with domain.
module Lycopene.Core.Event where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Monad (Lycopene, project, sprint, issue)
import qualified Lycopene.Core.Project as Project
import qualified Lycopene.Core.Sprint as Sprint
import qualified Lycopene.Core.Issue as Issue


-- Use cases of Project
-- =======================================

-- | Aggregation of Project use-case
-- Lift ordinary values to Project semantices
data Event a where
  -- | Lift Name and Description into a volatile entity.
  NewProject
    :: Name
    -> Description
    -> Event Project.Project
  -- | Fetch a project which identified by ProjectId
  FetchProject
    :: Project.ProjectId
    -> Event Project.Project
  -- | Fetch list of all project regardless of its status.
  AllProject
    :: Event [Project.Project]
  -- | Remove a project by specified name from domain space.
  RemoveProject
    :: Project.ProjectId 
    -> Event ()
  -- | Create a new sprint for the project.
  NewSprint
    :: Name
    -> Description
    -> Maybe Date
    -> Maybe Date
    -> Project.ProjectId
    -> Event Sprint.Sprint
  -- | Fetch all sprint deriving from the project.
  FetchProjectSprint
    :: Project.ProjectId
    -> Event [Sprint.Sprint]
  -- | 
  FetchSprint
    :: Sprint.SprintId
    -> Event Sprint.Sprint
  -- | 
  NewIssue
    :: Name
    -> Description
    -> Project.ProjectId
    -> Maybe Sprint.SprintId
    -> Event Issue.Issue
  -- | 
  FetchIssues
    :: Project.ProjectId
    -> Maybe Sprint.SprintId
    -> Issue.IssueStatus
    -> Event [Issue.Issue]
  -- |
  FetchIssue
    :: Issue.IssueId
    -> Event Issue.Issue
  RemoveIssue
    :: Issue.IssueId
    -> Event ()

processEvent :: Event a -> Lycopene a
processEvent (NewProject n d) = do
  pj <- project $ Project.addProject $ Project.newProject n d
  _ <- sprint $ Sprint.newBacklog (Project.projectId pj)
  return pj

processEvent (RemoveProject i) =
  () <$ (project $ Project.removeProject i)

processEvent (FetchProject i) =
  project $ Project.fetchProject i

processEvent AllProject =
  project $ Project.fetchAllProject

processEvent (FetchProjectSprint i) =
  sprint $ Sprint.fetchByStatusSprint i Sprint.SprintRunning

processEvent (FetchSprint i) =
  sprint $ Sprint.fetchSprint i

processEvent (NewIssue n d pj Nothing) = do
  sp <- sprint $ Sprint.fetchBacklogSprint pj
  issue $ Issue.addIssue pj (Sprint.sprintId sp)
             $ Issue.newIssue n d
processEvent (NewIssue n d pj (Just sp)) = do
  issue $ Issue.addIssue pj sp
             $ Issue.newIssue n d

processEvent (FetchIssues pj (Just sp) st) =
  issue $ Issue.fetchByStatusIssue pj sp st
processEvent (FetchIssues pj Nothing st) = do
  sp <- fmap Sprint.sprintId (sprint $ Sprint.fetchBacklogSprint pj)
  issue $ Issue.fetchByStatusIssue pj sp st

processEvent (FetchIssue i) = do
  issue $ Issue.fetchIssue i

processEvent (RemoveIssue i) = do
  issue $ Issue.removeIssue i
