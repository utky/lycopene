module Lycopene.Option.Version (version) where


import           Options.Applicative
import           Control.Monad.Trans.Class (lift)
import qualified Paths_lycopene as P
import           Data.Version (showVersion)

import           Lycopene.Option.Command
import           Lycopene.Process



version :: ParserInfo Command
version = info (pure Version) (progDesc "Print appilcation version infomation")

