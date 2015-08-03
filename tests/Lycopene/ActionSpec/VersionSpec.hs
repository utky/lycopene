module Lycopene.ActionSpec.VersionSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.ActionSpec.Internal

import           Lycopene.Action
import           Lycopene.Action.Version
import qualified Paths_lycopene as P
import           Data.Version (showVersion)

spec :: Spec
spec = do
  describe "Version" $ do
    it "return version" $ do
      version `shouldSuccess` showVersion P.version
