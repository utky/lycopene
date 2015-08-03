module Lycopene.ActionSpec.InitSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.ActionSpec.Internal

import           Lycopene.Action
import           Lycopene.Action.Init

spec :: Spec
spec = do
  describe "Init" $ do
    it "create project and return new project id 1" $ do
      initialize (Just "new-project") Nothing "." `shouldSuccess` 1


