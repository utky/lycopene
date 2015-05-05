module Lycopene.Process.Version where

import qualified Paths_lycopene as P
import           Data.Version (showVersion)
import           Lycopene.Process.Core (ProcessR, out)


version :: (Monad m) => ProcessR m
version = out . showVersion $ P.version
