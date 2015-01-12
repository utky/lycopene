module Lycopene.Option.Version (version) where


import           Options.Applicative
import           Control.Monad.Trans.Class (lift)
import qualified Paths_lycopene as P
import           Data.Version (showVersion)

import           Lycopene.Option.Command



version :: ParserInfo LycoAction
version = info versionP (progDesc "Print appilcation version infomation")
  where
    versionP = mkAction runVersion
    runVersion = return $ showVersion P.version

