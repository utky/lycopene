module Lycopene.ActionSpec.ConfigureSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.ActionSpec.Internal

import           Lycopene.Action
import           Lycopene.Action.Configure
import           Lycopene.Core.Project


spec :: Spec
spec = do
  describe "Configure" $ do
    it "return created inbox" $ do
      let exp = Project 0 inboxProjectName (Just inboxProjectDesc)
          actionBody = configure >> domain allProjects
          action = runAction config actionBody
      handleResult action `shouldSuccess` [exp]

