module Lycopene.Process.Ls where

import qualified Data.Text as T
import           Lycopene.Process.Internal
import           Lycopene.Configuration
import           Lycopene.Core.Issue as Issue

toTsv :: Issue.IssueR -> T.Text
toTsv r = T.pack ((show $ Issue.rIssueId r) ++ "\t" ++
            (Issue.rProjectName r) ++ "\t" ++
            (Issue.rSprintName r) ++ "\t" ++
            (Issue.rStatus r) ++ "\t" ++
            (Issue.rTitle r))

{-
listIssues :: (MonadIO m) => Bool -> ProcessR m
listIssues showAll  = do
  projectId <- targetProject `fmap` lift ask
  issues <- runDomain $ Issue.listOpenIssues projectId
  eachP issues
-}
listIssues = undefined
