module Lycopene.Configuration 
    ( Configuration(..)
    , defaultConfiguration
    ) where

data Configuration = Configuration
                   { lycoHome :: FilePath
                   , datapath :: String
                   , targetProject :: Integer
                   }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
                     { lycoHome = "."
                     , datapath = ":memory:"
                     , targetProject = 0
                     }
