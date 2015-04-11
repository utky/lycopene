module Lycopene.Process
    ( processCommand
    , module Lycopene.Process.Internal
    , module Lycopene.Process.Version
    ) where

import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..))
import           Lycopene.Process.Internal (LycoError(..), Result(..), Chunk, ProcessM, Process, runProcess, out, debug)
import           Lycopene.Process.Version (version)

processCommand :: LycoCommand -> Process
processCommand (LycoCommand _ Version) = version
processCommand _ = undefined

