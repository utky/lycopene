module Lycopene.ProcessSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Monad.Trans (liftIO)
import           Lycopene.Process

spec :: Spec
spec = do
  describe "Process" $ do
    it "version" $ do 
      -- runEffect $ (for (yield 1 >-> printer) (liftIO . assertEqual "yield string 1" "1")) >-> await
      pendingWith "not implemented"
