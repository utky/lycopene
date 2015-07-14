module Lycopene.Action.New where

import           Lycopene.Action
import           Lycopene.Core
import           Lycopene.Core.Issue as Issue


newIssue :: String -> Maybe String -> Action ()
newIssue title mDesc = domain $ do
  pjId <- targetProject `fmap` context 
  _ <- Issue.newIssue IssueRequest 
                      { irTitle = title
                      , irDescription = mDesc
                      , irProjectId = pjId
                      }
  return ()
