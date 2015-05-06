module Lycopene.Core.Issue.Service where

import           Data.Maybe (fromMaybe)
import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Issue.Entity as E
import qualified Lycopene.Core.Sprint as Sprint

data IssueRequest = IssueRequest
                  { irTitle :: String
                  , irDescription :: Maybe String
                  , irProjectId :: Integer
                  }

newIssue :: IssueRequest -> LycopeneT Persist Integer
newIssue ir = do
  sprint <- fromMaybe 0 `fmap` Sprint.getBacklogSprint (irProjectId ir)
  liftL $ insertP E.insertIssueV E.IssueV
                                 { E.vTitle = irTitle ir
                                 , E.vDescription = irDescription ir
                                 , E.vSprintId = sprint
                                 , E.vStatus = E.openStatus
                                 }

listOpenIssues :: Integer -> LycopeneT Persist [E.IssueR]
listOpenIssues pjId = liftL $ relationP E.openIssues (pjId, E.openStatus)

