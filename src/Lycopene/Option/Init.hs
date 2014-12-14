module Lycopene.Option.Init
    ( initDB
    ) where 

import           Options.Applicative

import           Lycopene.Core
import           Lycopene.Option.Command

initDB :: ParserInfo LycoAction
initDB = info initP (progDesc "initialize database if it doesn't exist.")
  where
    initP = mkAction initDatabase

