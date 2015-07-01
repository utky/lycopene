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

newIssue :: IssueRequest -> Lycopene Integer
newIssue ir = do
  sprint <- fromMaybe 0 `fmap` Sprint.getBacklogSprint (irProjectId ir)
  runPersist $ insertP E.insertIssueV E.IssueV
                                 { E.vTitle = irTitle ir
                                 , E.vDescription = irDescription ir
                                 , E.vSprintId = sprint
                                 , E.vStatus = E.openStatus
                                 }

listOpenIssues :: Integer -> Lycopene [E.IssueR]
listOpenIssues pjId = runPersist $ relationP E.openIssues (pjId, E.openStatus)

