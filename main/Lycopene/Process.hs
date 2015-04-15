{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( processCommand
    , module Lycopene.Process.Core
    , module Lycopene.Process.Version
    ) where

import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..))
import           Lycopene.Process.Core (LycoError(..), Result(..), Chunk, ProcessM, runProcess, out, debug, MonadIO)
import           Lycopene.Process.Version (version)
import           Lycopene.Process.Configure (configure)

processCommand :: (MonadIO m) => LycoCommand -> ProcessM m
processCommand (LycoCommand _ Version) = version
processCommand (LycoCommand _ (Configure target)) = configure target
processCommand _ = undefined

