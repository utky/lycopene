module Lycopene.Configuration 
    ( Configuration(..)
    ) where

data Configuration = Configuration
                   { lycoHome :: FilePath
                   , datapath :: String
                   }

