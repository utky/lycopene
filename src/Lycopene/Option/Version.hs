module Lycopene.Option.Version (version) where


import Options.Applicative
import Control.Monad.Trans.Class (lift)

import Lycopene.Option.Command



version :: ParserInfo LycoAction
version = info versionP (progDesc "Print appilcation version infomation")
  where
    versionP = mkAction runVersion
    runVersion = lift $ putStrLn "version 0.0.0"

