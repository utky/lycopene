module Lycopene.Option.Common
        ( CommonOption(..)
        , commonOption
        , commonOptsWithHome
        ) where

import           System.FilePath
import           Options.Applicative
import           Lycopene.Configuration

home :: FilePath
home = "~" ++ [pathSeparator]


data CommonOption = CommonOption
                   { verbose :: Bool
                   , homeLocation :: FilePath
                   , global :: Bool
                   } deriving (Show)

commonOption :: Parser CommonOption
commonOption = CommonOption <$> optionP <*> lycoHomeP <*> globalP where
  optionP = switch
          (  short 'v'
          <> long "verbose"
          <> help "Set to print verbosely ")
  lycoHomeP = option auto
          (  short 'c'
          <> long "context"
          <> help "Set lycopene home directory path as context"
          <> value ("~" </> ".lyco" :: FilePath) )
  globalP = switch
          (  short 'g'
          <> long "global"
          <> help "Use global context") 

type HomePath = FilePath

commonOptsWithHome :: CommonOption -> HomePath -> CommonOption
commonOptsWithHome commonops hp = commonops { homeLocation = expandHome } where
  expandHome = replaceHome (homeLocation commonops) hp

replaceHome :: FilePath -> HomePath -> FilePath
replaceHome fp hp
  | (firstdir fp) == home = hp </> (tailPath fp)
  | otherwise = fp
  where firstdir = head . splitPath
        tailPath = joinPath . tail . splitPath

