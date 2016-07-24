{-# LANGUAGE GADTs #-}
module Lycopene.Core.Issue where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Sprint (SprintId)

type IssueId = Identifier

data IssueStatus
  = Open
  | Close
  deriving (Show, Eq, Ord)

data Issue
    = Issue
    { issueId :: !Identifier
    , issueTitle :: !String
    , issueDescription :: !Description
    , issueStatus :: !IssueStatus
    } deriving (Show)

instance Eq Issue where
    x == y = (issueId x) == (issueId y)

-- | Aggregation of Issue use-case
data IssueEvent a where
  -- | Create new issue for the sprint.
  NewIssue :: String -> Description -> SprintId -> IssueEvent IssueId
  -- | Remove the issue from sprint.
  RemoveIssue :: IssueId -> IssueEvent ()
  -- | Update issue status.
  UpdateIssueStatus :: IssueStatus -> IssueId -> IssueEvent ()
  -- | Fetch current open issue.
  FetchOpenIssue :: SprintId -> IssueEvent [Issue]
  -- | Move the issue to given sprint.
  MoveIssue :: SprintId -> IssueId -> IssueEvent ()
