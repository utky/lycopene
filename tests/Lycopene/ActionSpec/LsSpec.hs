module Lycopene.ActionSpec.LsSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.ActionSpec.Internal

import           Lycopene.Core.Issue (IssueR(..))
import           Lycopene.Action
import           Lycopene.Action.Ls

spec :: Spec
spec = do
  describe "Ls" $ do
    it "returns empty list when there is no issue." $ do
      withDB (listIssues True) `shouldSuccess` []
