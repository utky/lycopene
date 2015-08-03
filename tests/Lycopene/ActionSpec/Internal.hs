module Lycopene.ActionSpec.Internal where

import           Test.Hspec
import           Lycopene.Environment (createDatabase)
import           Lycopene.Configuration
import           Lycopene.Action
import           Lycopene.Action.Configure
import           Lycopene.Core

import           Debug.Trace (trace)

config :: Configuration
config = defaultConfiguration
         { datapath = ":memory:"
         }

withDB :: Action a -> Action a
withDB = (>>) configure

runWithDB :: Action a -> IO (Either LycoError a)
runWithDB x = handleResult $ runAction config (withDB x) 

shouldSuccess :: (Eq a, Show a) => Action a -> a -> Expectation
fa `shouldSuccess` a = do
  e <- handleResult $ runAction config (fmap (`shouldBe` a) fa)
  either (\_ -> expectationFailure "hahaha") id e
