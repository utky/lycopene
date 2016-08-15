module Lycopene.Configuration 
    ( module Lycopene.Configuration.Data
    , loadConfig
    ) where

import           Lycopene.Configuration.Data (Configuration(..))
import           Data.Yaml (decodeEither)
import qualified Data.ByteString as B

loadConfig :: FilePath -> IO (Either String Configuration)
loadConfig p = fmap decodeEither $ B.readFile p
