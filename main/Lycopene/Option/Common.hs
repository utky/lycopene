module Lycopene.Option.Common
        ( CommonOption(..)
        , commonOption
        , configure
        ) where

import           System.FilePath
import           Options.Applicative
import           Lycopene.Configuration

data CommonOption = CommonOption
                   { verbose :: Bool
                   , homeLocation :: FilePath
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
 
configure :: CommonOption -> Configuration
configure co = Configuration
               { lycoHome = homeLocation co
               }


