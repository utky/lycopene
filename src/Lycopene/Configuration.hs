module Lycopene.Configuration 
    ( Configuration(..)
    ) where

import           Data.Char (isDigit)
import           System.FilePath
import           System.Directory

data Configuration = Configuration
                   { lycoHome :: FilePath
                   , datapath :: String
                   , targetProject :: Integer
                   }
