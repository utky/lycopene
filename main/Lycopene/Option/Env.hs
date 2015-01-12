module Lycopene.Option.Env
        ( mkLycoEnv
        ) where

import           Data.Maybe (fromMaybe)
import           System.Directory
import           System.FilePath
import           System.Environment (lookupEnv)

type SystemEnv = [(String, String)]

data LycoEnv = LycoEnv 
             { lycohome :: FilePath
             }

home :: FilePath
home = "~" ++ [pathSeparator]

defaultHome = home </> ".lyco"

mkLycoEnv :: SystemEnv -> LycoEnv
mkLycoEnv se = LycoEnv lh where
  lh = lookupWithDefault "LYCO_HOME" se defaultHome

lookupWithDefault :: Eq a => a -> [(a, c)] -> c -> c
lookupWithDefault = curry (fromMaybe' . (uncurry lookup)) where
  fromMaybe' = flip fromMaybe
