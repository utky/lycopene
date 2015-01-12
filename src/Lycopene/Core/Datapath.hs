module Lycopene.Core.Datapath
                ( mkDatapath
                , tempDatapath
                ) where

import           System.Environment (lookupEnv)
import           Data.Maybe (fromMaybe)
import           System.FilePath ((</>), pathSeparator)

home :: FilePath
home = "~" ++ [pathSeparator]

defaultHome = home </> ".lyco"


mkDatapath :: FilePath -> FilePath
mkDatapath basedir = basedir </> "issues.db"

tempDatapath :: FilePath
tempDatapath = ":memory:"

lookupWithDefault :: Eq a => a -> [(a, c)] -> c -> c
lookupWithDefault = curry (fromMaybe' . (uncurry lookup)) where
  fromMaybe' = flip fromMaybe
