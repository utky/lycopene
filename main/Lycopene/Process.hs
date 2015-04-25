{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( processCommand
    , module Lycopene.Process.Core
    , module Lycopene.Process.Version
    , module Lycopene.Process.Configure
    ) where

import           Control.Monad.Trans (lift)
import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..))
import           Lycopene.Process.Core (LycoError(..), Result(..), Chunk, ProcessM, Process, runProcess, out, debug, MonadIO)
import           Lycopene.Process.Version (version)
import           Lycopene.Process.Configure (configure)
import           Lycopene.Configuration (Configuration(..))
import           Lycopene.Core (config)

processCommand :: LycoCommand -> Process
processCommand (LycoCommand _ Version) = version
processCommand (LycoCommand _ Configure) = lift config >>= configure . datapath
processCommand _ = undefined

