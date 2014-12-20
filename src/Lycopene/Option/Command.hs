module Lycopene.Option.Command
    ( LycoCommand (..)
    , LycoAction (..)
    , CommonOption (..)
    , commonOption
    , runLycoCommand
    , mkAction
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Options.Applicative
import System.Directory
import System.FilePath

import Lycopene.Core
import Lycopene.Configuration

mkAction :: Show a => LycopeneT IO a -> Parser LycoAction
mkAction m = pure $ LycoAction printOutput where
  printOutput = m >>= liftIO . print

data LycoCommand = LycoCommand CommonOption LycoAction

newtype LycoAction = LycoAction { runAction :: LycopeneT IO () }

data CommonOption = CommonOption
                   { verbose :: Bool
                   , homeLocation :: FilePath
                   }
                  
type HomePath = FilePath

home :: FilePath
home = "~" ++ [pathSeparator]

replaceHome :: FilePath -> HomePath -> FilePath
replaceHome fp hp
  | (firstdir fp) == home = hp </> (tailPath fp)
  | otherwise = fp
  where firstdir = head . splitPath
        tailPath = joinPath . tail . splitPath

configure :: CommonOption -> Configuration
configure co = Configuration
               { lycoHome = homeLocation co
               }


commonOption :: Parser CommonOption
commonOption = CommonOption <$> optionP <*> lycoHomeP where
  optionP = switch
          (  short 'v'
          <> long "verbose"
          <> help "Set to print verbosely ")
  lycoHomeP = option auto
          (  short 'c'
          <> long "context"
          <> help "Set lycopene home directory path as context"
          <> value ("~" </> ".lyco" :: FilePath) )
 
runLycoCommand :: LycoCommand -> IO ()
runLycoCommand (LycoCommand c ac) = config >>= (runWithConfiguration ac) where
  runWithConfiguration = runLycopeneT . runAction
  config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory
  commonOptsWithHome commonops hp = commonops { homeLocation = expandHome commonops hp }
  expandHome commonops hp = replaceHome (homeLocation commonops) hp


