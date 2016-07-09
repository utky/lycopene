module Lycopene.Core.Issue where

import           Lycopene.Core.Scalar

data IssueStatus = Open | Close deriving (Show, Eq, Ord)

data Issue
    = Issue
    { issueId :: !Identifier
    , issueTitle :: !String
    , issueDescription :: !Description
    , issueStatus :: !IssueStatus
    } deriving (Show)

instance Eq Issue where
    x == y = (issueId x) == (issueId y)
