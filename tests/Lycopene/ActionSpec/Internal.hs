module Lycopene.ActionSpec.Internal where

import           Test.Hspec
import           Lycopene.Environment (createDatabase)
import           Lycopene.Configuration
import           Lycopene.Action
import           Lycopene.Action.Configure
import           Lycopene.Core
import           Control.Monad.Trans

import           Debug.Trace (trace)

config :: Configuration
config = defaultConfiguration
         { datapath = ":memory:"
         }

withDB :: Action a -> Action a
withDB = (>>) configure

runWithDB :: Action a -> IO (Either LycoError a)
runWithDB x = handleResult $ runAction config (withDB x) 


shouldBeInDomain :: (Eq a, Show a) => Action a -> a -> Action Expectation
shouldBeInDomain fa b =
  let traceFa x = trace (show x) x
      shouldBeDom = domain . return . (`shouldBe` b) . traceFa
  in  fa >>= shouldBeDom

shouldSuccess :: (Eq a, Show a) => Action a -> a -> Expectation
fa `shouldSuccess` a = do
  let expectation = fa `shouldBeInDomain` a
      result = runAction config expectation
  e <- handleResult result
  either (expectationFailure . show) id e
