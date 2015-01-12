module Lycopene.Core.Database
        ( initDatabase
        ) where

import           Lycopene.Core.Monad
import           Lycopene.Core.Query


initDatabase :: LycopeneT IO ()
initDatabase = do
  c <- getConfig
  liftL $ runCreateTables c

