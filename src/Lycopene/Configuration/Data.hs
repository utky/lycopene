{-# LANGUAGE DeriveGeneric #-}
module Lycopene.Configuration.Data
    ( Configuration(..)
    ) where

import           Data.Yaml (ToJSON, FromJSON)
import           GHC.Generics

data Configuration = Configuration
                   { dummy :: String
                   , schema :: String
                   } deriving (Generic)

instance ToJSON Configuration
instance FromJSON Configuration
