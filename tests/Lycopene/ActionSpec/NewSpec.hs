module Lycopene.ActionSpec.NewSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.ActionSpec.Internal

import           Lycopene.Core
import           Lycopene.Core.Issue (IssueR(..))
import           Lycopene.Action
import           Lycopene.Action.Ls
import           Lycopene.Action.New


spec :: Spec
spec = do
  describe "New" $ do
    it "creates a new issue and it can be queried." $ do
      let expected = IssueR
             { rIssueId = 1
             , rProjectName = "global"
             , rSprintName = "inbox"
             , rStatus = "open"
             , rTitle = "issue title"
             }
      (newIssue (rTitle expected) Nothing >> listIssues True) `shouldSuccess` [expected]

