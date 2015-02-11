module Lycopene.Configuration 
    ( Configuration(..)
    ) where

data Configuration = Configuration
                   { lycoHome :: FilePath
                   , datapath :: String
                   , contextName :: Maybe String -- | A name of contextual project
                   }

