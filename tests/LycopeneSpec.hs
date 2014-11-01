module LycopeneSpec (spec) where

import Test.Hspec

import Lycopene
import Lycopene.Configuration

spec :: Spec
spec = do
  describe "lycopene" $ do
    it "stub" $ do
      "Hoge" `shouldBe` "Hoge"
       
