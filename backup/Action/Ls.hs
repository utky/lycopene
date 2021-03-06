module Lycopene.Action.Ls where

import           Lycopene.Action
import           Lycopene.Core
import qualified Lycopene.Core.Issue as Issue


listIssues :: Bool -> Action [Issue.IssueR]
listIssues showAll  = domain $ do
  projectId <- targetProject `fmap` context
  Issue.listOpenIssues projectId
