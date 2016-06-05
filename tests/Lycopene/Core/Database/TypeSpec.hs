{-# LANGUAGE GADTs #-}
module Lycopene.Core.Database.TypeSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck

import           Lycopene.Core.Database.Type

newtype Output = Output { unOutput :: String } deriving (Eq, Show)

{-| Domain specific queries -}
data Query a where
  SelectById :: Int -> Query Output
  SelectAll :: Query [Output]

runQuery :: (Monad m) => Query a -> m a
runQuery (SelectById i) = return (Output (show i))
runQuery (SelectAll)    = return (map Output [])

selectById :: Int -> Fetch Query Output
selectById = fetch . SelectById

selectAll :: Fetch Query [Output]
selectAll = fetch SelectAll

spec :: Spec
spec = do
  describe "Fetch" $ do
    it "can be compared equality" $ property $
      \x -> (runFetch runQuery (selectById x)) `shouldReturn` (Output show (x :: Int))

