module Lycopene.Process.Version where

import qualified Paths_lycopene as P
import           Data.Version (showVersion)
import           Lycopene.Action


version :: Action String
version = return . showVersion $ P.version
