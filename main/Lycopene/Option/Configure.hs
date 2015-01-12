module Lycopene.Option.Configure
    ( configureDB
    ) where 

import           Options.Applicative

import           Lycopene.Core
import           Lycopene.Option.Command

configureDB :: ParserInfo LycoAction
configureDB = info initP (progDesc "initialize database if it doesn't exist.")
  where
    initP = mkAction initDatabase

