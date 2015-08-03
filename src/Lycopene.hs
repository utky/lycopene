module Lycopene
    ( lycopene
    , module Lycopene.Configuration
    ) where

import           Lycopene.Configuration
import           Lycopene.Option
import           Lycopene.Process



type Arguments = [String]

lycopene :: Arguments ->  Configuration -> IO ()
lycopene args conf = execParserWithArgs (lycoParser conf) args >>= runCommand conf

