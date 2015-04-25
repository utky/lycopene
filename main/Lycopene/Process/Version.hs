module Lycopene.Process.Version where

import qualified Paths_lycopene as P
import           Data.Version (showVersion)
import           Lycopene.Process.Core (Process, out)


version :: Process
version = out . showVersion $ P.version
