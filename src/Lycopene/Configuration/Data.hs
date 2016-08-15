{-# LANGUAGE DeriveGeneric #-}
module Lycopene.Configuration.Data
    ( Configuration(..)
    , defaultConfiguration
    ) where

import           Data.Yaml (ToJSON, FromJSON)
import           GHC.Generics

data Configuration = Configuration
                   { datapath :: String
                   }

instance ToJSON Configuration
instance FromJSON Configuration
