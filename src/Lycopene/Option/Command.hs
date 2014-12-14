module Lycopene.Option.Command
    ( LycoCommand (..)
    , LycoAction (..)
    , CommonOption (..)
    , commonOption
    , runLycoCommand
    , mkAction
    ) where

import Control.Applicative
import Options.Applicative
import System.FilePath

import Lycopene.Core
import Lycopene.Configuration

mkAction :: LycopeneT IO () -> Parser LycoAction
mkAction = pure . LycoAction

data LycoCommand = LycoCommand CommonOption LycoAction

newtype LycoAction = LycoAction { runAction :: LycopeneT IO () }

data CommonOption = CommonOption
                   { verbose :: Bool
                   , homeLocation :: FilePath
                   }

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
          <> help "Set lycopene home directory path as context"
          <> value ("~" </> ".lyco" :: FilePath) )
 
runLycoCommand :: LycoCommand -> IO ()
runLycoCommand (LycoCommand c action) = runWithConfiguration action config where
  runWithConfiguration = runLycopeneT . runAction
  config = configure c

