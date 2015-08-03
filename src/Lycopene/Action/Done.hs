module Lycopene.Action.Done where

import           Control.Monad (void)
import           Lycopene.Action
import           Lycopene.Core.Issue as Issue


doneIssue :: Integer -> Action ()
doneIssue = domain . void . Issue.closeIssue
