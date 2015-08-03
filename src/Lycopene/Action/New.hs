module Lycopene.Action.New where

import           Lycopene.Action
import           Lycopene.Core
import           Lycopene.Core.Issue as Issue


newIssue :: String -> Maybe String -> Action ()
newIssue _title mDesc = domain $ do
  pjId <- targetProject `fmap` context 
  _ <- Issue.newIssue IssueRequest 
                      { irTitle = _title
                      , irDescription = mDesc
                      , irProjectId = pjId
                      }
  return ()
