module Lycopene.Environment where

import           Lycopene.Core.Database
import           Lycopene.Core.Monad

createDatabase :: LycopeneT Persist ()
createDatabase = liftL $ direct createTables
