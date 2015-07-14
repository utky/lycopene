module Lycopene.ActionSpec.Internal where

import           Test.Hspec
import           Lycopene.Environment (createDatabase)
import           Lycopene.Configuration
import           Lycopene.Action
import           Lycopene.Action.Configure
import           Lycopene.Core

import           Debug.Trace (trace)

config :: Configuration
config = Configuration "." ":memory:" 0

runWithDB :: Action a -> IO (Either LycoError a)
runWithDB x = handleResult $ runAction config (configure >> x) 

x `shouldSuccess` y = x `shouldReturn` Right y
