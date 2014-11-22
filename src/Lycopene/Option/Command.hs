module Lycopene.Option.Command
    ( LycoCommand (..)
    , LycoSubcommand (..)
    , CommonOption (..)
    , commonOption
    , runLycoCommand
    ) where

import Control.Applicative
import Options.Applicative
import System.FilePath

import Lycopene.Configuration

data LycoCommand = LycoCommand CommonOption LycoSubcommand

newtype LycoSubcommand = LycoSubcommand 
                         { runSubcommand :: Configuration -> IO ()
                         }

data CommonOption = CommonOption
                   { verbose :: Bool
                   , lycoHome :: FilePath
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
runLycoCommand (LycoCommand c s) = runWithConfiguration configure where
  runWithConfiguration = runSubcommand s
  configure = Configuration

