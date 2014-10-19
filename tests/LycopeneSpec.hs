module LycopeneSpec (spec) where

import Test.Hspec

import Lycopene
import Lycopene.Configuration

spec :: Spec
spec = do
  describe "lycopene" $ do
    it "returns Hoge" $ do
      runLycopene (Configuration "hoge") `shouldBe` "Hoge"
