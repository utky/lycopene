module Lycopene.Process.Version where

import qualified Paths_lycopene as P
import           Data.Version (showVersion)
import           Lycopene.Process.Core (ProcessM, out, MonadIO)


version :: (MonadIO m) => ProcessM m
version = out . showVersion $ P.version
