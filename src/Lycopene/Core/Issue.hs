module Lycopene.Core.Issue () where

data Issue = Issue
           { issueId :: Key
           , issueTitle :: String
           } deriving (Show)

