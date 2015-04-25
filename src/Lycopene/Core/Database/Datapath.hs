module Lycopene.Core.Database.Datapath
                ( tempDatapath
                ) where

-- import           Data.Maybe (fromMaybe)
-- import           System.FilePath ((</>), pathSeparator)
import           System.FilePath ((</>))

-- home :: FilePath
-- home = "~" ++ [pathSeparator]

-- defaultHome = home </> ".lyco"


tempDatapath :: FilePath
tempDatapath = ":memory:"

-- lookupWithDefault :: Eq a => a -> [(a, c)] -> c -> c
-- lookupWithDefault = curry (fromMaybe' . (uncurry lookup)) where
--   fromMaybe' = flip fromMaybe
